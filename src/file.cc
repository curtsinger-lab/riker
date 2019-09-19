#include "file.hh"

#include <set>

#include <dirent.h>
#include <fcntl.h>
#include <linux/magic.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/vfs.h>
#include <unistd.h>

#include "blake2-wrapper.h"
#include "db.capnp.h"

#include "middle.h"

File::File(Trace& trace, size_t location, bool is_pipe, kj::StringPtr path, Command* creator,
           File* prev_version) :
    _trace(trace),
    _location(location),
    _serialized(_trace.temp_message.getOrphanage().newOrphan<db::File>()),
    _version(0),
    creator(creator),
    writer(nullptr),
    prev_version(prev_version) {
  // TODO: consider using orphans to avoid copying
  if (is_pipe) {
    _serialized.get().setType(db::FileType::PIPE);
  } else {
    _serialized.get().setType(db::FileType::REGULAR);
    _serialized.get().setPath(path);
  }
}

// return a set of the commands which raced on this file, back to the parameter version
std::set<Command*> File::collapse(unsigned int version) {
  // this->has_race = true;
  File* cur_file = this;
  std::set<Command*> conflicts;
  while (cur_file->getVersion() != version) {
    // add writer and all readers to conflict set
    if (cur_file->writer != nullptr) {
      conflicts.insert(cur_file->writer);
    }
    for (auto rd : cur_file->getInteractors()) {
      conflicts.insert(rd);
    }
    // add all mmaps to conflicts
    for (auto m : cur_file->_mmaps) {
      conflicts.insert(m->getCommand());
    }
    // step back a version
    cur_file = cur_file->prev_version;
  }
  return conflicts;
}

File* File::createVersion() {
  // We are at the end of the current version, so snapshot with a fingerprint
  fingerprint();
  _serialized.get().setLatestVersion(false);

  File* f = new File(_trace, _location, _serialized.getReader().getType() == db::FileType::PIPE,
                     _serialized.getReader().getPath(), creator, this);
  f->_version = _version + 1;

  _trace.files.insert(f);
  _trace.latest_versions[this->_location] = f;
  return f;
}

bool File::shouldSave() {
  // Save files that have at least one reader
  if (!_readers.empty()) return true;

  // Save files with a writer
  if (isWritten()) return true;

  // Save files with a creator
  if (isCreated()) return true;

  // Save files with a previous version that are not removed (CC: why?)
  if (prev_version != nullptr && !isRemoved()) return true;

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
template <typename OutType>
static bool blake2sp_dir(std::string path_string, OutType checksum_output) {
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
    return false;
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
  blake2sp_final(&hash_state, checksum_output.begin(), checksum_output.size());
  return true;
}

// Compute the blake2sp checksum of a file's contents
// Return true on success
template <typename OutType>
static bool blake2sp_file(std::string path_string, OutType checksum_output) {
  int file_fd = open(path_string.c_str(), O_RDONLY | O_CLOEXEC);
  if (file_fd < 0) {  // Error opening
    return false;
  }

  blake2sp_state hash_state;
  blake2sp_init(&hash_state, BLAKE2S_OUTBYTES);
  char buffer[1 << 13];
  while (true) {
    auto bytes_read = read(file_fd, buffer, ARRAY_COUNT(buffer));
    if (bytes_read > 0) {
      blake2sp_update(&hash_state, buffer, bytes_read);
    } else if (bytes_read == 0) {  // EOF
      blake2sp_final(&hash_state, checksum_output.begin(), checksum_output.size());
      close(file_fd);
      return true;
    } else {  // Error reading
      close(file_fd);
      return false;
    }
  }
}

void File::fingerprint() {
  auto builder = _serialized.get();

  // We can only fingerprint regular files for now. (Do we even want to try for others?)
  if (builder.getType() != db::FileType::REGULAR) {
    builder.setFingerprintType(db::FingerprintType::UNAVAILABLE);
    return;
  }

  auto path_string = getPath();
  struct stat stat_info;
  if (stat(path_string.cStr(), &stat_info) != 0) {
    if (errno == ENOENT) {
      builder.setFingerprintType(db::FingerprintType::NONEXISTENT);
    } else {
      builder.setFingerprintType(db::FingerprintType::UNAVAILABLE);
    }
    return;
  }

  builder.setSize((uint64_t)stat_info.st_size);

  auto mod_time = builder.initModificationTime();
  mod_time.setSecondsSinceEpoch(stat_info.st_mtim.tv_sec);
  mod_time.setNanoseconds(stat_info.st_mtim.tv_nsec);

  builder.setInode(stat_info.st_ino);
  builder.setMode(stat_info.st_mode);

  // Skip checksum if the file has no users
  if (getReaders().size()) {
    builder.setFingerprintType(db::FingerprintType::METADATA_ONLY);
    return;
  }

  // Set up to record a checksum
  auto checksum = builder.initChecksum(BLAKE2S_OUTBYTES);

  // Is this a file or a directory?
  if ((stat_info.st_mode & S_IFMT) == S_IFDIR) {
    // Checksum a directory
    if (blake2sp_dir(path_string, checksum)) {
      builder.setFingerprintType(db::FingerprintType::BLAKE2SP);
    } else {
      // Accessing contents failed
      builder.setFingerprintType(db::FingerprintType::METADATA_ONLY);
    }
    return;

  } else if ((stat_info.st_mode & S_IFMT) == S_IFREG) {
    // Checksum a file
    if (blake2sp_file(path_string, checksum)) {
      builder.setFingerprintType(db::FingerprintType::BLAKE2SP);
    } else {
      // Accessing contents failed
      builder.setFingerprintType(db::FingerprintType::METADATA_ONLY);
    }
    return;
  } else {
    // Don't checksum other types of files
    builder.setFingerprintType(db::FingerprintType::METADATA_ONLY);
    return;
  }
}

void File::serialize(db::File::Builder builder) {
  auto r = _serialized.getReader();
  builder.setPath(r.getPath());
  builder.setType(r.getType());
  builder.setMode(r.getMode());
  builder.setFingerprintType(r.getFingerprintType());
  builder.setSize(r.getSize());
  builder.setModificationTime(r.getModificationTime());
  builder.setInode(r.getInode());
  builder.setChecksum(r.getChecksum());
  builder.setLatestVersion(r.getLatestVersion());
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

    // Make space for the checksum output
    size_t hash_size = file.getChecksum().size();
    KJ_STACK_ARRAY(kj::byte, checksum, hash_size, hash_size, hash_size);

    // Is this a file or a directory?
    if ((stat_info.st_mode & S_IFMT) == S_IFDIR) {
      // Checksum a directory
      if (!blake2sp_dir(path_string, checksum)) {
        // Checksum failed. Return mismatch
        return false;
      }

    } else if ((stat_info.st_mode & S_IFMT) == S_IFREG) {
      // Checksum a regular file
      if (!blake2sp_file(path_string, checksum)) {
        // Checksum failed. Return mismatch
        return false;
      }
    } else {
      // Some other file type
      return false;
    }

    // Compare checksums
    return checksum.asConst() == file.getChecksum();

  } else {
    // Unrecognized fingerprint type
    return false;
  }
}
