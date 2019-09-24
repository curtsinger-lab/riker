#include "core/File.hh"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <set>
#include <string>
#include <vector>

#include <dirent.h>
#include <fcntl.h>
#include <linux/magic.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <unistd.h>

#include <capnp/blob.h>
#include <kj/string.h>

#include "core/BuildGraph.hh"
#include "core/Process.hh"
#include "db/db.capnp.h"
#include "fingerprint/blake2.hh"

File::File(BuildGraph& graph, size_t location, bool is_pipe, std::string path,
           std::shared_ptr<Command> creator) :
    _graph(graph),
    _location(location),
    _path(path),
    _creator(creator) {
  if (is_pipe) {
    _type = db::FileType::PIPE;
  } else {
    _type = db::FileType::REGULAR;
  }
}

std::shared_ptr<File> File::createVersion(std::shared_ptr<Command> creator) {
  // We are at the end of the current version, so snapshot with a fingerprint
  fingerprint();

  std::shared_ptr<File> f = std::make_shared<File>(_graph, _location, isPipe(), getPath(), creator);
  _graph.addFile(f);
  f->_version++;
  f->_prev_version = shared_from_this();
  _next_version = f;
  _graph.setLatestVersion(_location, f);
  return f;
}

std::shared_ptr<File> File::getLatestVersion() {
  if (_next_version)
    return _next_version->getLatestVersion();
  else
    return shared_from_this();
}

bool File::isModified() const {
  // Files that were created cannot be modified (modifications would make a new version)
  if (isCreated()) return false;

  // Files that do not have previous version cannot be modified
  if (!hasPreviousVersion()) return false;

  // The file is only modified if the previous version was not removed
  return !getPreviousVersion()->isRemoved();

  // This is a simplification of a previous check that I believe was wrong.
  // The check is copied below in case there is a problem in the future or some missed detail:
  //
  //(!file->isCreated() && file->getPreviousVersion() != nullptr &&
  //                       (file->getPreviousVersion()->isCreated() ||
  //                        (file->getPreviousVersion()->getPreviousVersion() != nullptr &&
  //                         !file->getPreviousVersion()->isRemoved())));
}

// return a set of the commands which raced on this file, back to the parameter version
std::set<std::shared_ptr<Command>> File::collapse(unsigned int version) {
  std::shared_ptr<File> cur_file = shared_from_this();
  std::set<std::shared_ptr<Command>> conflicts;
  while (cur_file->getVersion() != version) {
    // add writer and all readers to conflict set
    if (cur_file->isWritten()) {
      conflicts.insert(cur_file->getWriter());
    }
    for (auto rd : cur_file->getInteractors()) {
      conflicts.insert(rd);
    }
    // add all mmaps to conflicts
    for (auto m : cur_file->_mmaps) {
      conflicts.insert(m->getCommand());
    }
    // step back a version
    cur_file = cur_file->getPreviousVersion();
  }
  return conflicts;
}

bool File::shouldSave() {
  // Save files that have at least one reader
  if (!_readers.empty()) return true;

  // Save files with a writer
  if (isWritten()) return true;

  // Save files with a creator
  if (isCreated()) return true;

  // Save files with a previous version that are not removed (CC: why?)
  if (hasPreviousVersion() && !isRemoved()) return true;

  // Skip anything else
  return false;
}

// Filter out db.dodo from the directory listing when fingerprinting directories
static int filter_default(const struct dirent* e) {
  if (std::string(e->d_name) == "db.dodo")
    return 0;
  else
    return 1;
}

// For NFS filesystems, filter out .nfs files
static int filter_nfs(const struct dirent* e) {
  if (std::string(e->d_name).compare(0, 4, ".nfs") == 0)
    return 0;
  else
    return filter_default(e);
}

// Compute the blake2sp checksum of a directory's contents
// Return true on success
static std::vector<uint8_t> blake2sp_dir(std::string path_string) {
  std::vector<uint8_t> checksum;

  // Select a directory filter based on filesystem
  auto filter = filter_default;

  // Use statfs to check for NFS
  struct statfs fs;
  if (statfs(path_string.c_str(), &fs) == 0) {
    if (fs.f_type == NFS_SUPER_MAGIC) {
      filter = filter_nfs;
    }
  }

  // Get a sorted list of directory entries
  struct dirent** namelist;
  int entries = scandir(path_string.c_str(), &namelist, filter, alphasort);

  // Did reading the directory fail?
  if (entries == -1) {
    return checksum;
  }

  // Start a checksum
  blake2sp_state hash_state;
  blake2sp_init(&hash_state, BLAKE2S_OUTBYTES);

  // Add each directory entry into the checksum
  for (int i = 0; i < entries; i++) {
    // Hash the entry name and null terminator to mark boundaries between entries
    blake2sp_update(&hash_state, namelist[i]->d_name, strlen(namelist[i]->d_name) + 1);
    free(namelist[i]);
  }

  // Clean up the name list
  free(namelist);

  // Finish the checksum
  checksum.resize(BLAKE2S_OUTBYTES);
  blake2sp_final(&hash_state, checksum.data(), checksum.size());
  return checksum;
}

// Compute the blake2sp checksum of a file's contents
// Return true on success
static std::vector<uint8_t> blake2sp_file(std::string path_string) {
  std::vector<uint8_t> checksum;

  int file_fd = open(path_string.c_str(), O_RDONLY | O_CLOEXEC);
  if (file_fd < 0) {  // Error opening
    return checksum;
  }

  blake2sp_state hash_state;
  blake2sp_init(&hash_state, BLAKE2S_OUTBYTES);
  char buffer[1 << 13];
  while (true) {
    auto bytes_read = read(file_fd, buffer, sizeof(buffer));
    if (bytes_read > 0) {
      blake2sp_update(&hash_state, buffer, bytes_read);
    } else if (bytes_read == 0) {  // EOF
      checksum.resize(BLAKE2S_OUTBYTES);
      blake2sp_final(&hash_state, checksum.data(), checksum.size());
      close(file_fd);
      return checksum;
    } else {  // Error reading
      close(file_fd);
      return checksum;
    }
  }
}

void File::fingerprint() {
  // We can only fingerprint regular files for now. (Do we even want to try for others?)
  if (getType() != db::FileType::REGULAR) {
    _fingerprint_type = db::FingerprintType::UNAVAILABLE;
    return;
  }

  auto path_string = getPath();

  // Stat the file to get metadata
  if (stat(path_string.c_str(), &_stat_info) != 0) {
    if (errno == ENOENT) {
      _fingerprint_type = db::FingerprintType::NONEXISTENT;
    } else {
      _fingerprint_type = db::FingerprintType::UNAVAILABLE;
    }

    return;
  }

  // At least for now, we only have a metadata fingerprint
  _fingerprint_type = db::FingerprintType::METADATA_ONLY;

  // Skip checksum if the file has no users
  if (getReaders().size()) return;

  // Is this a file or a directory?
  if ((_stat_info.st_mode & S_IFMT) == S_IFDIR) {
    // Checksum a directory
    _checksum = blake2sp_dir(path_string);

    // If the checksum succeeded, update to a blake2 fingerprint
    if (_checksum.size()) _fingerprint_type = db::FingerprintType::BLAKE2SP;

  } else if ((_stat_info.st_mode & S_IFMT) == S_IFREG) {
    // Checksum a file
    _checksum = blake2sp_file(path_string);

    // If the checksum succeeded, update to a blake2 fingerprint
    if (_checksum.size()) _fingerprint_type = db::FingerprintType::BLAKE2SP;
  }
}

void File::serialize(db::File::Builder builder) {
  builder.setPath(getPath());
  builder.setType(getType());
  builder.setMode(getMode());
  builder.setLatestVersion(isLatestVersion());

  builder.setFingerprintType(_fingerprint_type);

  if (_checksum.size()) {
    auto output_checksum = builder.initChecksum(_checksum.size());
    memcpy(output_checksum.begin(), _checksum.data(), _checksum.size());
  }

  builder.setSize(_stat_info.st_size);
  builder.setInode(_stat_info.st_ino);
  builder.setMode(_stat_info.st_mode);

  auto mod_time = builder.initModificationTime();
  mod_time.setSecondsSinceEpoch(_stat_info.st_mtim.tv_sec);
  mod_time.setNanoseconds(_stat_info.st_mtim.tv_nsec);
}

bool match_fingerprint(db::File::Reader file) {
  // First, check if the fingerprint type is something we recognize. If not, we conservatively
  // assume a change.
  switch (file.getFingerprintType()) {
    case db::FingerprintType::UNAVAILABLE:
    case db::FingerprintType::NONEXISTENT:
    case db::FingerprintType::METADATA_ONLY:
    case db::FingerprintType::BLAKE2SP:
      break;
    default:
      return false;
  }

  // If there is no fingerprint, assume no match
  if (file.getFingerprintType() == db::FingerprintType::UNAVAILABLE) {
    return false;
  }

  // All the rest of the fingerprint types require metadata.
  kj::StringPtr path_string = file.getPath();
  struct stat stat_info;
  if (stat(path_string.cStr(), &stat_info) != 0) {
    return (errno == ENOENT && file.getFingerprintType() == db::FingerprintType::NONEXISTENT);
  }

  // If the size is different, then the file is definitely different.
  if ((uint64_t)stat_info.st_size != file.getSize()) {
    return false;
  }
  // Otherwise, if the rest of the metadata matches, then we assume no change.
  if (file.getModificationTime().getSecondsSinceEpoch() == stat_info.st_mtim.tv_sec &&
      file.getModificationTime().getNanoseconds() == stat_info.st_mtim.tv_nsec &&
      file.getInode() == stat_info.st_ino && file.getMode() == stat_info.st_mode) {
    return true;
  }

  // If all we have to go on is the metadata, and the metadata is different, we have
  // to assume that the change is real.
  if (file.getFingerprintType() == db::FingerprintType::METADATA_ONLY) {
    return false;
  }

  // What type of fingerprint are we dealing with?
  if (file.getFingerprintType() == db::FingerprintType::BLAKE2SP) {
    // Using blake2sp

    std::vector<uint8_t> checksum;

    // Is this a file or a directory?
    if ((stat_info.st_mode & S_IFMT) == S_IFDIR) {
      // Checksum a directory
      checksum = blake2sp_dir(path_string);
      if (!checksum.size()) {
        // Checksum failed. Return mismatch
        return false;
      }

    } else if ((stat_info.st_mode & S_IFMT) == S_IFREG) {
      // Checksum a regular file
      checksum = blake2sp_file(path_string);
      if (!checksum.size()) {
        // Checksum failed. Return mismatch
        return false;
      }
    } else {
      // Some other file type
      return false;
    }

    // Compare checksums
    return memcmp(checksum.data(), file.getChecksum().begin(), checksum.size()) == 0;
  } else {
    // Unrecognized fingerprint type
    return false;
  }
}
