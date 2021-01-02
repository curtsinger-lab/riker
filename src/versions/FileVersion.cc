#include "FileVersion.hh"

#include <memory>
#include <optional>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "blake3.h"
#include "data/IRSink.hh"
#include "ui/constants.hh"
#include "ui/options.hh"
#include "util/log.hh"

// Is this version saved in a way that can be committed?
bool FileVersion::canCommit() const noexcept {
  if (isCommitted()) return true;
  return _empty || (options::enable_cache && _cached);
}

/// Commit this version to the filesystem
void FileVersion::commit(fs::path path) noexcept {
  if (isCommitted()) return;

  ASSERT(canCommit()) << "Attempted to commit unsaved version " << this << " to " << path;

  // is this an empty file?
  if (_empty) {
    // stage in empty file
    commitEmptyFile(path);
    return;
  }

  // did we cache the file?
  if (_cached) {
    // stage in cached file
    stage(path);
    return;
  }

  FAIL << "Committable file version " << this << " must be either empty or cached.";
}

// Commit this version to the filesystem
void FileVersion::commitEmptyFile(fs::path path, mode_t mode) noexcept {
  if (isCommitted()) return;

  ASSERT(canCommit()) << "Attempted to commit unsaved version " << this << " to " << path;

  int fd = ::open(path.c_str(), O_WRONLY | O_CREAT | O_TRUNC, mode);
  FAIL_IF(fd < 0) << "Failed to commit empty file version: " << ERR;
  close(fd);

  LOG(artifact) << "Created file at " << path;

  // Mark this version as committed
  Version::setCommitted();
}

/// Save a fingerprint of this version
void FileVersion::fingerprint(fs::path path) noexcept {
  // if there is already a fingerprint with a hash, move on
  if (hasHash()) return;

  // does the file actually exist?
  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);
  if (rc != 0) return;  // leave mtime and hash undefined

  // otherwise, take a "fingerprint"
  // save mtime from statbuf
  _empty = statbuf.st_size == 0;
  _mtime = statbuf.st_mtim;

  // if file is readable and hashable, save hash
  if (rc == 0) {
    // is this a regular file?
    if (!(statbuf.st_mode & S_IFREG)) return;
    _b3hash = blake3(path);

    // if caching is enabled, cache the file
    if (options::enable_cache && _b3hash.has_value()) {
      cache(path);
    }
  }
}

/// Does this FileVersion have a hash already?
bool FileVersion::hasHash() noexcept {
  return _b3hash.has_value();
}

void FileVersion::makeEmptyFingerprint() noexcept {
  // it is not necessary to fingerprint or cache empty files
  _empty = true;
}

/// Restores a file to the given path from the cache
void FileVersion::stage(fs::path path) noexcept {
  // Does the staged file already exist?  If so, don't stage.
  // struct stat statbuf;
  // bool stage_exists(::lstat(path.c_str(), &statbuf) == 0);
  // if (stage_exists) return;

  // Path to cache file
  fs::path hash_file = cacheFilePath();

  // Get the length of the cached file
  struct stat cache_statbuf;
  bool file_exists(::lstat(hash_file.c_str(), &cache_statbuf) == 0);
  ASSERT(file_exists) << "Unable to stage in '" << path << "' because cache file '" << hash_file
                      << "' does not exist: " << ERR;
  loff_t len = cache_statbuf.st_size;

  // Open source and destination fds
  int src_fd = ::open(hash_file.c_str(), O_RDONLY);
  FAIL_IF(src_fd == -1) << "Unable to open cache file '" << hash_file << "': " << ERR;
  int dst_fd = ::open(path.c_str(), O_CREAT | O_TRUNC | O_WRONLY);  // TODO: no mode makes me sad
  FAIL_IF(dst_fd == -1) << "Unable to create stage file '" << path << "': " << ERR;

  // copy file to cache using non-POSIX fast copy
  loff_t bytes_cp;
  do {
    bytes_cp = ::copy_file_range(src_fd, NULL, dst_fd, NULL, len, 0);
    FAIL_IF(bytes_cp == -1) << "Could not copy cache file '" << hash_file << "' to stage location '"
                            << path << "': " << ERR;

    len -= bytes_cp;
  } while (len > 0 && bytes_cp > 0);

  LOG(artifact) << "Staged in file version at path '" << path << "' from cache file '" << hash_file
                << "'";

  close(src_fd);
  close(dst_fd);
}

void FileVersion::cache(fs::path path) noexcept {
  // Don't cache if the fingerprint is missing
  if (!_b3hash.has_value()) return;

  // Don't cache files that weren't created by the build
  if (!getCreator()) return;

  // Path to cache file
  fs::path hash_file = cacheFilePath(_b3hash.value());
  fs::path hash_dir = hash_file.parent_path();

  // Get the length of the input file
  struct stat statbuf;
  if (::lstat(path.c_str(), &statbuf)) {
    LOG(artifact) << "Failed to stat " << path;
    return;
  }
  loff_t len = statbuf.st_size;

  // Does the cache file already exist?  If so, skip. (reuse statbuf)
  struct stat cache_statbuf;
  bool file_exists(::lstat(hash_file.c_str(), &cache_statbuf) == 0);
  if (file_exists) return;

  // Create the directories, if needed
  fs::create_directories(hash_dir);

  // Open source and destination fds
  int src_fd = ::open(path.c_str(), O_RDONLY);
  FAIL_IF(src_fd == -1) << "Unable to open file '" << path << "' for caching: " << ERR;
  int dst_fd = ::open(hash_file.c_str(), O_CREAT | O_EXCL | O_WRONLY, 0600);
  FAIL_IF(dst_fd == -1) << "Unable to create cache file '" << hash_file << "' for file '" << path
                        << "': " << ERR;

  // copy file to cache using non-POSIX fast copy
  loff_t bytes_cp;
  do {
    bytes_cp = ::copy_file_range(src_fd, NULL, dst_fd, NULL, len, 0);
    FAIL_IF(bytes_cp == -1) << "Could not copy file '" << path << "' to cache location '"
                            << hash_file << "': " << ERR;

    len -= bytes_cp;
  } while (len > 0 && bytes_cp > 0);

  LOG(artifact) << "Cached file version at path '" << path << "' in '" << hash_file << "'";

  close(src_fd);
  close(dst_fd);

  _cached = true;
}

/// Compare to another fingerprint instance
bool FileVersion::fingerprints_match(shared_ptr<FileVersion> other) const noexcept {
  // Two empty files are always equivalent
  if (_empty && other->_empty) {
    LOG(artifact) << "Not checking equality for fingerprint: both files are empty.";
    return true;
  }

  // Do the mtimes match?
  if (_mtime.has_value() && other->_mtime.has_value()) {
    auto m1 = _mtime.value();
    auto m2 = other->_mtime.value();
    if (m1.tv_sec == m2.tv_sec && m1.tv_nsec == m2.tv_nsec) {
      // Yes. Return a match immediately
      return true;
    }
  }

  // If fingerprinting is enabled, check to see if we have a hash and the hashes match
  if (!options::mtime_only && _b3hash.has_value() && other->_b3hash.has_value() &&
      _b3hash.value() == other->_b3hash.value()) {
    return true;
  }

  // If fingerprinting is disabled but the hashes match, print some info
  if (options::mtime_only && _b3hash.has_value() && other->_b3hash.has_value() &&
      _b3hash.value() == other->_b3hash.value()) {
  }

  return false;
}

/// Return a BLAKE3 hash for the contents of the file at the given path (static method)
std::optional<BLAKE3Hash> FileVersion::blake3(fs::path path) noexcept {
  // initialize hasher
  blake3_hasher hasher;
  blake3_hasher_init(&hasher);

  // buffer for file read
  unsigned char buf[BLAKE3BUFSZ];

  // read from given file
  LOG(exec) << "Fingerprinting " << path;
  FILE* f = fopen(path.c_str(), "r");
  if (!f) {
    LOG(artifact) << "Unable to fingerprint file '" << path.c_str() << "': " << ERR;
    return nullopt;
  }

  // create output array
  BLAKE3Hash output;

  // compute hash incrementally for each chunk read
  ssize_t n;
  while ((n = fread(buf, sizeof(char), sizeof(buf), f)) > 0) {
    blake3_hasher_update(&hasher, buf, n);
  }
  fclose(f);

  // finalize the hash
  blake3_hasher_finalize(&hasher, output.data(), BLAKE3_OUT_LEN);

  return output;
}

/// Convert a BLAKE3 byte array to a hexadecimal string (static method)
string FileVersion::b3hex(BLAKE3Hash b3hash) noexcept {
  stringstream ss;
  for (int byte : b3hash) {
    ss << std::setfill('0') << std::setw(2) << std::hex << byte;
  }
  return ss.str();
}

/// Return the path for the contents of this cached FileVersion
fs::path FileVersion::cacheFilePath(BLAKE3Hash& hash) noexcept {
  // We use a three-level directory prefix scheme to store cached files
  // to avoid having too many files in a given folder.  This scheme
  // below has 16^6 unique directory prefixes.
  string hash_str = b3hex(hash);
  fs::path dir_lvl_0 = hash_str.substr(0, 2);
  fs::path dir_lvl_1 = hash_str.substr(2, 2);
  fs::path dir_lvl_2 = hash_str.substr(4, 2);
  fs::path hash_dir = constants::CacheDir / dir_lvl_0 / dir_lvl_1 / dir_lvl_2;

  // Path to cache file
  return hash_dir / hash_str;
}

/// Return the path for the contents of this cached FileVersion
fs::path FileVersion::cacheFilePath() noexcept {
  ASSERT(_b3hash.has_value()) << "Cannot obtain cache location for unfingerprinted file.";
  return cacheFilePath(_b3hash.value());
}

/// Pretty printer
ostream& FileVersion::print(ostream& o) const noexcept {
  // is empty
  if (_empty) return o << "[file content: empty]";

  // not empty, no mtime, no hash
  if (!_mtime.has_value() && !_b3hash.has_value()) return o << "[file content: unknown]";

  // has mtime
  o << "[file content: ";
  if (_mtime.has_value())
    o << "mtime=" << _mtime.value().tv_sec << "." << std::setfill('0') << std::setw(9)
      << _mtime.value().tv_nsec << " ";

  // has hash
  if (_b3hash.has_value()) o << "b3hash=" << b3hex();

  o << "]";

  return o;
}

/// get a string representation of the hash
string FileVersion::b3hex() const noexcept {
  if (!_b3hash.has_value()) {
    return "NO HASH";
  }
  return b3hex(_b3hash.value());
}

/// Compare this version to another version
bool FileVersion::matches(shared_ptr<Version> other) const noexcept {
  auto other_file = other->as<FileVersion>();
  if (!other_file) return false;
  if (other_file.get() == this) return true;
  return fingerprints_match(other_file);
}