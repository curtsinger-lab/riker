#include "FileVersion.hh"

#include <memory>
#include <optional>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "blake3.h"
#include "interfaces/TraceHandler.hh"
#include "util/log.hh"

// Is this version saved in a way that can be committed?
bool FileVersion::canCommit() const noexcept {
  if (isCommitted()) return true;
  return _empty;
}

// Commit this version to the filesystem
void FileVersion::commitWithMode(fs::path path, mode_t mode) noexcept {
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
void FileVersion::fingerprint(fs::path path, fs::path cache_dir) noexcept {
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
      cache(statbuf, _b3hash.value(), path, cache_dir);
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

void FileVersion::cache(const struct stat& statbuf,
                        BLAKE3Hash& hash,
                        fs::path path,
                        fs::path cache_dir) noexcept {
  // Don't cache files that weren't created by the build
  if (!getCreator()) return;

  // Path to cache file
  fs::path hash_file = cacheFilePath(hash, cache_dir);
  fs::path hash_dir = hash_file.parent_path();

  // Get the length of the input file
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
  int dst_fd = ::open(hash_file.c_str(), O_CREAT | O_EXCL | O_WRONLY);
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
      LOG(artifact) << "mtimes match.";
      return true;
    }
  }

  // If fingerprinting is enabled, check to see if we have a hash and the hashes match
  if (!options::mtime_only && _b3hash.has_value() && other->_b3hash.has_value() &&
      _b3hash.value() == other->_b3hash.value()) {
    LOG(artifact) << "Fingerprints match";
    return true;
  }

  // If fingerprinting is disabled but the hashes match, print some info
  if (options::mtime_only && _b3hash.has_value() && other->_b3hash.has_value() &&
      _b3hash.value() == other->_b3hash.value()) {
    LOG(artifact) << "Fingerprints match, but mtimes do not";
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
fs::path FileVersion::cacheFilePath(BLAKE3Hash& hash, fs::path cache_dir) noexcept {
  // We use a three-level directory prefix scheme to store cached files
  // to avoid having too many files in a given folder.  This scheme
  // below has 16^6 unique directory prefixes.
  string hash_str = b3hex(hash);
  fs::path dir_lvl_0 = hash_str.substr(0, 2);
  fs::path dir_lvl_1 = hash_str.substr(2, 2);
  fs::path dir_lvl_2 = hash_str.substr(4, 2);
  fs::path hash_dir = cache_dir / dir_lvl_0 / dir_lvl_1 / dir_lvl_2;

  // Path to cache file
  return hash_dir / hash_str;
}

/// Return the path for the contents of this cached FileVersion relative to the given cache_dir
fs::path FileVersion::cacheFilePath(fs::path cache_dir) noexcept {
  ASSERT(_b3hash.has_value()) << "Cannot obtain cache location for unfingerprinted file.";
  return cacheFilePath(_b3hash.value(), cache_dir);
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