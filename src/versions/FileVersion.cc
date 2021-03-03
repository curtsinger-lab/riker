#include "FileVersion.hh"

#include <cerrno>
#include <filesystem>
#include <iomanip>
#include <memory>
#include <optional>
#include <sstream>
#include <string>

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "blake3.h"
#include "ui/constants.hh"
#include "ui/options.hh"
#include "util/log.hh"
#include "util/wrappers.hh"

using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::stringstream;

namespace fs = std::filesystem;

// The number of bytes read from a file at once when using read() for blake3 hashing
enum : size_t { BLAKE3BUFSZ = 65536 };

/// Convert a BLAKE3 byte array to a hexadecimal string
static string b3hex(FileVersion::Hash b3hash) noexcept {
  stringstream ss;
  for (int byte : b3hash) {
    ss << std::setfill('0') << std::setw(2) << std::hex << byte;
  }
  return ss.str();
}

/// Return a BLAKE3 hash for the contents of the file at the given path.
static optional<FileVersion::Hash> blake3(fs::path path, struct stat& statbuf) noexcept {
  // initialize hasher
  blake3_hasher hasher;
  blake3_hasher_init(&hasher);

  // read from given file
  LOG(exec) << "Fingerprinting " << path;
  int fd = ::open(path.c_str(), O_RDONLY);
  if (fd < 0) {
    LOG(artifact) << "Unable to fingerprint file " << path << ": " << ERR;
    return nullopt;
  }

  // create output array
  FileVersion::Hash output;

  // try to mmap the file
  void* p = ::mmap(nullptr, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);

  // Did the mmap call succeed?
  if (p == MAP_FAILED) {
    // No. Fall back to read() calls
    LOG(artifact) << "Unable to mmap file " << path << ": " << ERR;

    // Buffer for reading blocks from the file
    char buf[BLAKE3BUFSZ];

    // compute hash incrementally for each chunk read
    ssize_t n;
    while ((n = ::read(fd, buf, sizeof(buf))) > 0) {
      blake3_hasher_update(&hasher, buf, n);
    }

  } else {
    LOG(artifact) << "Hashing file " << path << " from mmapped data.";
    // Yes. Now p points to the file data. Send it all at once.
    blake3_hasher_update(&hasher, p, statbuf.st_size);

    // Unmap
    ::munmap(p, statbuf.st_size);
  }

  // Close the file
  ::close(fd);

  // finalize the hash
  blake3_hasher_finalize(&hasher, output.data(), BLAKE3_OUT_LEN);

  return output;
}

/// Generate a path from a hash value. The result does not include the cache directory path.
static fs::path hashPath(FileVersion::Hash& hash) noexcept {
  // We use a three-level directory prefix scheme to store cached files
  // to avoid having too many files in a given folder.  This scheme
  // below has 16^6 unique directory prefixes.
  string hash_str = b3hex(hash);
  fs::path dir_lvl_0 = hash_str.substr(0, 2);
  fs::path dir_lvl_1 = hash_str.substr(2, 2);
  fs::path dir_lvl_2 = hash_str.substr(4, 2);
  return dir_lvl_0 / dir_lvl_1 / dir_lvl_2 / hash_str;
}

/// Tell the garbage collector to preserve this version.
void FileVersion::gcLink() noexcept {
  // If this file is not cached, there's nothing to do.
  if (!_cached) return;

  // If this file is already linked into the new cache, bail
  if (_linked) return;

  // no fingerprint, no cache file, so bail
  FAIL_IF(!_hash.has_value()) << "Invalid gcLink on uncached file.";

  // Generate the hash path
  auto hash_path = hashPath(_hash.value());

  // get cache paths
  auto new_hash_file = constants::NewCacheDir / hash_path;
  auto cur_hash_file = constants::CacheDir / hash_path;

  // Create the directories, if needed
  fs::path new_hash_dir = new_hash_file.parent_path();
  fs::create_directories(new_hash_dir);

  // and then link the file in the old cache to the new cache
  int rv = ::link(cur_hash_file.c_str(), new_hash_file.c_str());

  // Check the return value
  if (rv == 0) {
    // If link returned 0, the link succeeded
    LOG(cache) << "Linked old cached file " << cur_hash_file << " to new cached file "
               << new_hash_file;

    _linked = true;

  } else if (rv == -1 && errno == EEXIST) {
    // If link return EEXIST, the cached file was already linked
    LOG(cache) << "Cache version " << this << " was already linked in the new cache";
    _linked = true;

  } else {
    FAIL << "Cannot link old cache file " << cur_hash_file << " into new cache location "
         << new_hash_file << ": " << ERR;
  }
}

// Is this version saved in a way that can be committed?
bool FileVersion::canCommit() const noexcept {
  return _empty || (options::enable_cache && _cached);
}

/// Commit this version to the filesystem
void FileVersion::commit(fs::path path) noexcept {
  // Compare to the on-disk artifact. This is a short-term workaround for lazy builds
  // TODO: Remove this once artifacts track both committed and uncommitted state
  if (options::lazy_builds && !canCommit()) {
    struct stat statbuf;
    ::lstat(path.c_str(), &statbuf);

    if (_mtime.has_value()) {
      if (_mtime.value().tv_sec == statbuf.st_mtim.tv_sec &&
          _mtime.value().tv_nsec == statbuf.st_mtim.tv_nsec) {
        return;
      }
    }

    if (_hash.has_value()) {
      auto fs_hash = blake3(path, statbuf);

      if (fs_hash == _hash.value()) {
        return;
      }
    }
  }

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
  ASSERT(canCommit()) << "Attempted to commit unsaved version " << this << " to " << path;

  int fd = ::open(path.c_str(), O_WRONLY | O_CREAT | O_TRUNC, mode);
  FAIL_IF(fd < 0) << "Failed to commit empty file version: " << ERR;
  close(fd);

  LOG(artifact) << "Created file at " << path;
}

/// Save a fingerprint of this version
void FileVersion::fingerprint(fs::path path, FingerprintType type) noexcept {
  // If no fingerprint was requested, return immediately
  if (type == FingerprintType::None) return;

  // If a quick fingerprint was requested and we already have mtime, return immediately
  if (type == FingerprintType::Quick && _mtime.has_value()) return;

  // If a full fingerprint was requested and we already have an mtime and hash, return immediately
  if (type == FingerprintType::Full && _mtime.has_value() && _hash.has_value()) return;

  // Stat the file to get mtime, empty, and size
  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);
  if (rc) {
    LOG(cache) << "Failed stat call in FileVersion::fingerprint(" << path << "): " << ERR;
    return;
  }

  // Update the empty and mtime fields
  _empty = statbuf.st_size == 0;
  _mtime = statbuf.st_mtim;

  // If a full fingerprint was requested and we don't have one already, collect it
  if (type == FingerprintType::Full && !_hash.has_value()) {
    // if file is a not regular file, bail
    if (!(statbuf.st_mode & S_IFREG)) return;

    // finally save hash
    _hash = blake3(path, statbuf);

    LOG(cache) << "Collected full fingerprint for version " << this << " at path " << path << ".";
  }
}

void FileVersion::makeEmptyFingerprint() noexcept {
  // it is not necessary to fingerprint or cache empty files
  _empty = true;
}

/// Restores a file to the given path from the cache.
/// Returns true if the cache file exists and restoration was successful.
/// The exact error message can be printed by the caller by inspecting errno.
bool FileVersion::stage(fs::path path) noexcept {
  // Make sure we have a hash and that this version is cached
  ASSERT(_hash.has_value()) << "Un-hashed file version " << this << " cannot be staged from cache";
  ASSERT(_cached) << "Attempted to stage un-cached file version " << this << " from cache.";

  // Path to cached file
  fs::path hash_file = constants::CacheDir / hashPath(_hash.value());

  // does the cached file exist, and if so, how big is it in bytes?
  off_t len = fileLength(hash_file);
  bool file_exists = len != -1;

  // the call to stage must succeed
  FAIL_IF(!file_exists) << "Unable to stage in cached file " << path << " from cached file "
                        << hash_file << ": " << ERR;

  // Open source and destination fds
  int src_fd = ::open(hash_file.c_str(), O_RDONLY);
  FAIL_IF(src_fd == -1) << "Unable to open cache file " << hash_file << ": " << ERR;
  int dst_fd = ::open(path.c_str(), O_CREAT | O_TRUNC | O_WRONLY, 0644);
  FAIL_IF(dst_fd == -1) << "Unable to create stage file " << path << ": " << ERR;

  // copy file to cache using non-POSIX fast copy
  loff_t bytes_cp;
  do {
    bytes_cp = ::copy_file_range(src_fd, NULL, dst_fd, NULL, len, 0);
    FAIL_IF(bytes_cp == -1) << "Could not copy cache file " << hash_file << " to stage location "
                            << path << ": " << ERR;

    len -= bytes_cp;
  } while (len > 0 && bytes_cp > 0);

  close(src_fd);
  close(dst_fd);

  LOG(cache) << "Staged in file version at path " << path << " from cache file " << hash_file;

  return true;
}

void fast_copy(fs::path src, fs::path dest) noexcept {
  // Get the length of the src file
  loff_t len = fileLength(src);
  FAIL_IF(len == -1) << "Failed to stat " << src << " for fast copy.";

  // Open source and destination fds
  int src_fd = ::open(src.c_str(), O_RDONLY);
  FAIL_IF(src_fd == -1) << "Unable to open file " << src << " for caching: " << ERR;
  int dst_fd = ::open(dest.c_str(), O_CREAT | O_EXCL | O_WRONLY, 0600);
  FAIL_IF(dst_fd == -1) << "Unable to create cache file '" << dest << " for file " << src << ": "
                        << ERR;

  // copy file to cache using non-POSIX fast copy
  loff_t bytes_cp;
  bool use_fallback_copy = false;
  do {
    bytes_cp = ::copy_file_range(src_fd, NULL, dst_fd, NULL, len, 0);

    // Did the copy fail?
    if (bytes_cp == -1) {
      // Yes. Was it because we're copying across devices?
      if (errno == EXDEV) {
        use_fallback_copy = true;
        break;
      } else {
        FAIL << "Could not copy file " << src << " to cache location '" << dest << ": " << ERR;
      }
    }

    len -= bytes_cp;
  } while (len > 0 && bytes_cp > 0);

  // Do we need to use a fallback copy?
  if (use_fallback_copy) {
    // Seek both files back to the beginning
    ::lseek(src_fd, 0, SEEK_SET);
    ::lseek(dst_fd, 0, SEEK_SET);

    // Copy chunks
    char buf[128];
    int bytes_read;
    while ((bytes_read = ::read(src_fd, buf, 128)) != 0) {
      int rc = ::write(dst_fd, buf, bytes_read);
      FAIL_IF(rc != bytes_read) << "Write failed during copy: " << ERR;
    }
  }

  close(src_fd);
  close(dst_fd);
}

void FileVersion::cache(fs::path path) noexcept {
  // Don't cache if already cached
  if (_cached) {
    LOG(artifact) << "Not caching version " << this << " at path " << path
                  << " because it is already cached.";
    return;
  }

  // Don't cache if this file is empty
  if (_empty) {
    LOG(artifact) << "Not caching version " << this << " at path " << path
                  << " because it is empty.";
    return;
  }

  // Make sure we have a full fingerprint for this version
  fingerprint(path, FingerprintType::Full);

  // Freak out if the fingerprint is missing
  if (!_hash.has_value()) {
    WARN << "Cannot cache version " << this << " at path " << path << " without a fingerprint.";
    return;
  }

  // Path to cache file
  fs::path hash_file = constants::CacheDir / hashPath(_hash.value());
  fs::path hash_dir = hash_file.parent_path();

  // Is the cache file already in the current cache?  If so, we're done.
  if (fileExists(hash_file)) {
    _cached = true;
    return;
  }

  // Otherwise, we need to cache the file

  // Create the directories, if needed
  fs::create_directories(hash_dir);

  // Copy the file, fast hopefully
  fast_copy(path, hash_file);

  LOG(artifact) << "Cached file version at path " << path << " in " << hash_file;

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
  if (!options::mtime_only && _hash.has_value() && other->_hash.has_value() &&
      _hash.value() == other->_hash.value()) {
    return true;
  }

  // If fingerprinting is disabled but the hashes match, print some info
  if (options::mtime_only && _hash.has_value() && other->_hash.has_value() &&
      _hash.value() == other->_hash.value()) {
  }

  return false;
}

/// Pretty printer
ostream& FileVersion::print(ostream& o) const noexcept {
  // is empty
  if (_empty) return o << "[file content: empty]";

  // not empty, no mtime, no hash
  if (!_mtime.has_value() && !_hash.has_value()) return o << "[file content: unknown]";

  // has mtime
  o << "[file content: ";
  if (_mtime.has_value())
    o << "mtime=" << _mtime.value().tv_sec << "." << std::setfill('0') << std::setw(9)
      << _mtime.value().tv_nsec << " ";

  // has hash
  if (_hash.has_value()) o << "b3hash=" << b3hex(_hash.value()) << " ";

  // has cached copy
  o << "cached=" << (_cached ? "true" : "false");

  o << "]";

  return o;
}

/// Compare this version to another version
bool FileVersion::matches(shared_ptr<ContentVersion> other) noexcept {
  if (!other) return false;
  auto other_file = other->as<FileVersion>();
  if (!other_file) return false;
  if (other_file.get() == this) return true;

  if (fingerprints_match(other_file)) {
    // If either version is hashed, propagate that to the other version
    if (_hash.has_value()) {
      other_file->_hash = _hash;
    } else if (other_file->_hash.has_value()) {
      _hash = other_file->_hash;
    }

    // If either file version is cached, propagate that to the other version
    if (_cached) {
      other_file->_cached = true;
    } else if (other_file->_cached) {
      _cached = true;
    }

    return true;
  }

  return false;
}
