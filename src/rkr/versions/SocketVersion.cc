#include "SocketVersion.hh"

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
#include "util/constants.hh"
#include "util/log.hh"
#include "util/options.hh"
#include "util/wrappers.hh"

using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::stringstream;

namespace fs = std::filesystem;

// The number of bytes read from a socket at once when using read() for blake3 hashing
enum : size_t { BLAKE3BUFSZ = 65536 };

/// Convert a BLAKE3 byte array to a hexadecimal string
static string b3hex(SocketVersion::Hash b3hash) noexcept {
  stringstream ss;
  for (int byte : b3hash) {
    ss << std::setfill('0') << std::setw(2) << std::hex << byte;
  }
  return ss.str();
}

/// Return a BLAKE3 hash for the contents of the socket at the given path.
static optional<SocketVersion::Hash> blake3(fs::path path, struct stat& statbuf) noexcept {
  // initialize hasher
  blake3_hasher hasher;
  blake3_hasher_init(&hasher);

  // read from given socket
  LOG(artifact) << "Fingerprinting " << path;
  int fd = ::open(path.c_str(), O_RDONLY);
  if (fd < 0) {
    LOG(artifact) << "Unable to fingerprint socket " << path << ": " << ERR;
    return nullopt;
  }

  // create output array
  SocketVersion::Hash output;

  // try to mmap the socket
  void* p = ::mmap(nullptr, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);

  // Did the mmap call succeed?
  if (p == MAP_FAILED) {
    // No. Fall back to read() calls
    LOG(artifact) << "Unable to mmap socket " << path << ": " << ERR;

    // Buffer for reading blocks from the socket
    char buf[BLAKE3BUFSZ];

    // compute hash incrementally for each chunk read
    ssize_t n;
    while ((n = ::read(fd, buf, sizeof(buf))) > 0) {
      blake3_hasher_update(&hasher, buf, n);
    }

  } else {
    LOG(artifact) << "Hashing socket " << path << " from mmapped data.";
    // Yes. Now p points to the socket data. Send it all at once.
    blake3_hasher_update(&hasher, p, statbuf.st_size);

    // Unmap
    ::munmap(p, statbuf.st_size);
  }

  // Close the socket
  ::close(fd);

  // finalize the hash
  blake3_hasher_finalize(&hasher, output.data(), BLAKE3_OUT_LEN);

  return output;
}

/// Generate a path from a hash value. The result does not include the cache directory path.
static fs::path hashPath(SocketVersion::Hash& hash) noexcept {
  // We use a three-level directory prefix scheme to store cached sockets
  // to avoid having too many sockets in a given folder.  This scheme
  // below has 16^6 unique directory prefixes.
  string hash_str = b3hex(hash);
  fs::path dir_lvl_0 = hash_str.substr(0, 2);
  fs::path dir_lvl_1 = hash_str.substr(2, 2);
  fs::path dir_lvl_2 = hash_str.substr(4, 2);
  return dir_lvl_0 / dir_lvl_1 / dir_lvl_2 / hash_str;
}

/// Tell the garbage collector to preserve this version.
void SocketVersion::gcLink() noexcept {
  // If this socket is not cached, there's nothing to do.
  if (!_cached) return;

  // If this socket is already linked into the new cache, bail
  if (_linked) return;

  // no fingerprint, no cache socket, so bail
  FAIL_IF(!_hash.has_value()) << "Invalid gcLink on uncached socket.";

  // Generate the hash path
  auto hash_path = hashPath(_hash.value());

  // get cache paths
  auto new_hash_socket = constants::NewCacheDir / hash_path;
  auto cur_hash_socket = constants::CacheDir / hash_path;

  // Create the directories, if needed
  fs::path new_hash_dir = new_hash_socket.parent_path();
  fs::create_directories(new_hash_dir);

  // and then link the socket in the old cache to the new cache
  int rv = ::link(cur_hash_socket.c_str(), new_hash_socket.c_str());

  // Check the return value
  if (rv == 0) {
    // If link returned 0, the link succeeded
    LOG(cache) << "Linked old cached socket " << cur_hash_socket << " to new cached socket "
               << new_hash_socket;

    _linked = true;

  } else if (rv == -1 && errno == EEXIST) {
    // If link return EEXIST, the cached socket was already linked
    LOG(cache) << "Cache version " << this << " was already linked in the new cache";
    _linked = true;

  } else {
    FAIL << "Cannot link old cache socket " << cur_hash_socket << " into new cache location "
         << new_hash_socket << ": " << ERR;
  }
}

// Is this version saved in a way that can be committed?
bool SocketVersion::canCommit() const noexcept {
  return _empty || (options::enable_cache && _cached);
}

/// Commit this version to the filesystem
void SocketVersion::commit(fs::path path, mode_t mode) noexcept {
  ASSERT(canCommit()) << "Attempted to commit unsaved version " << this << " to " << path;

  // is this an empty socket?
  if (_empty) {
    // stage in empty socket
    commitEmptySocket(path, mode);
    return;
  }

  // did we cache the socket?
  if (_cached) {
    // stage in cached socket
    stage(path, mode);
    return;
  }

  FAIL << "Committable socket version " << this << " must be either empty or cached.";
}

// Commit this version to the filesystem
void SocketVersion::commitEmptySocket(fs::path path, mode_t mode) noexcept {
  ASSERT(canCommit()) << "Attempted to commit unsaved version " << this << " to " << path;

  int fd = ::open(path.c_str(), O_WRONLY | O_CREAT | O_TRUNC, mode);
  FAIL_IF(fd < 0) << "Failed to commit empty socket version: " << ERR;
  close(fd);

  LOG(artifact) << "Created socket at " << path;
}

/// Save a fingerprint of this version
void SocketVersion::fingerprint(fs::path path, FingerprintType type) noexcept {
  // If no fingerprint was requested, return immediately
  if (type == FingerprintType::None) return;

  // If a quick fingerprint was requested and we already have mtime, return immediately
  if (type == FingerprintType::Quick && _mtime.has_value()) return;

  // If a full fingerprint was requested and we already have an mtime and hash, return immediately
  if (type == FingerprintType::Full && _mtime.has_value() && _hash.has_value()) return;

  // Stat the socket to get mtime, empty, and size
  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);
  if (rc) {
    LOG(cache) << "Failed stat call in SocketVersion::fingerprint(" << path << "): " << ERR;
    return;
  }

  // Update the empty and mtime fields
  _empty = statbuf.st_size == 0;
  _mtime = statbuf.st_mtim;

  // If a full fingerprint was requested and we don't have one already, collect it
  if (type == FingerprintType::Full && !_hash.has_value()) {
    // if socket is a not regular socket, bail
    if (!(statbuf.st_mode & S_IFREG)) return;

    // WARN << "Fingerprinting " << path;

    // finally save hash
    _hash = blake3(path, statbuf);

    LOG(cache) << "Collected full fingerprint for version " << this << " at path " << path << ".";
  }
}

void SocketVersion::makeEmptyFingerprint() noexcept {
  // it is not necessary to fingerprint or cache empty sockets
  _empty = true;
}

bool fast_socket_copy(fs::path src, fs::path dest, mode_t mode = 0600) noexcept {
  // Get the length of the src socket
  loff_t len = fileLength(src);
  if (len == -1) {
    WARN << "Failed to stat " << src << " for fast copy: " << ERR;
    return false;
  }

  // Open source and destination fds
  int src_fd = ::open(src.c_str(), O_RDONLY);
  if (src_fd == -1) {
    WARN << "Unable to open source socket " << src << ": " << ERR;
    return false;
  }

  int dst_fd = ::open(dest.c_str(), O_CREAT | O_TRUNC | O_WRONLY, mode);
  if (dst_fd == -1) {
    WARN << "Unable to create socket " << dest << ": " << ERR;
    return false;
  }

  // copy socket to cache using non-POSIX fast copy
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
        WARN << "Could not copy socket " << src << " to cache location '" << dest << ": " << ERR;
        return false;
      }
    }

    len -= bytes_cp;
  } while (len > 0 && bytes_cp > 0);

  // Do we need to use a fallback copy?
  if (use_fallback_copy) {
    // Seek both sockets back to the beginning
    ::lseek(src_fd, 0, SEEK_SET);
    ::lseek(dst_fd, 0, SEEK_SET);

    // Copy chunks
    char buf[128];
    int bytes_read;
    while ((bytes_read = ::read(src_fd, buf, 128)) != 0) {
      int rc = ::write(dst_fd, buf, bytes_read);
      if (rc != bytes_read) {
        WARN << "Write failed during copy: " << ERR;
        return false;
      }
    }
  }

  close(src_fd);
  close(dst_fd);

  return true;
}

/// Restores a socket to the given path from the cache.
/// Returns true if the cache socket exists and restoration was successful.
/// The exact error message can be printed by the caller by inspecting errno.
bool SocketVersion::stage(fs::path path, mode_t mode) noexcept {
  // Make sure we have a hash and that this version is cached
  ASSERT(_hash.has_value()) << "Un-hashed socket version " << this
                            << " cannot be staged from cache";
  ASSERT(_cached) << "Attempted to stage un-cached socket version " << this << " from cache.";

  // Path to cached socket
  fs::path hash_socket = constants::CacheDir / hashPath(_hash.value());

  // Copy the cached socket into place
  FAIL_IF(!fast_socket_copy(hash_socket, path, mode))
      << "Failed to stage socket " << path << " from cache";

  LOG(cache) << "Staged in socket version at path " << path << " from cache socket " << hash_socket;

  return true;
}

void SocketVersion::cache(fs::path path) noexcept {
  // Don't cache if already cached
  if (_cached) {
    LOG(artifact) << "Not caching version " << this << " at path " << path
                  << " because it is already cached.";
    return;
  }

  // Don't cache if this socket is empty
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

  // Path to cache socket
  fs::path hash_socket = constants::CacheDir / hashPath(_hash.value());
  fs::path hash_dir = hash_socket.parent_path();

  // Is the cache socket already in the current cache?  If so, we're done.
  if (fileExists(hash_socket)) {
    _cached = true;
    return;
  }

  // Otherwise, we need to cache the socket

  // Create the directories, if needed
  fs::create_directories(hash_dir);

  // Copy the socket, fast hopefully
  if (fast_socket_copy(path, hash_socket)) {
    LOG(artifact) << "Cached socket version at path " << path << " in " << hash_socket;
    _cached = true;
  }
}

/// Compare to another fingerprint instance
bool SocketVersion::fingerprints_match(shared_ptr<SocketVersion> other) const noexcept {
  // Two empty sockets are always equivalent
  if (_empty && other->_empty) {
    LOG(artifact) << "Not checking equality for fingerprint: both sockets are empty.";
    return true;
  }

  // Do we have hashes?
  if (_hash.has_value() && other->_hash.has_value()) {
    // Yes. Comparing them gives us a definitive answer on the match
    return _hash.value() == other->_hash.value();
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

  // If fingerprinting is disabled but the hashes match, print some info
  if (_hash.has_value() && other->_hash.has_value() && _hash.value() == other->_hash.value()) {
  }

  return false;
}

/// Pretty printer
ostream& SocketVersion::print(ostream& o) const noexcept {
  // is empty
  if (_empty) return o << "[socket content: empty]";

  // not empty, no mtime, no hash
  if (!_mtime.has_value() && !_hash.has_value()) return o << "[socket content: unknown]";

  // has mtime
  o << "[socket content: ";
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
bool SocketVersion::matches(shared_ptr<ContentVersion> other) noexcept {
  if (!other) return false;
  auto other_socket = other->as<SocketVersion>();
  if (!other_socket) return false;
  if (other_socket.get() == this) return true;

  if (fingerprints_match(other_socket)) {
    // If either version is hashed, propagate that to the other version
    if (_hash.has_value()) {
      other_socket->_hash = _hash;
    } else if (other_socket->_hash.has_value()) {
      _hash = other_socket->_hash;
    }

    // If either socket version is cached, propagate that to the other version
    if (_cached) {
      other_socket->_cached = true;
    } else if (other_socket->_cached) {
      _cached = true;
    }

    return true;
  }

  return false;
}

/*
bool operator!=(const struct timespec& t1, const struct timespec& t2) noexcept {
  return t1.tv_sec != t2.tv_sec || t1.tv_nsec != t2.tv_nsec;
}

/// Can a write of this version be coalesced with another?
bool SocketVersion::canCoalesceWith(std::shared_ptr<ContentVersion> other) const noexcept {
  // Is the other version a socket version? If not, the writes cannot be coalesced
  auto other_fv = other->as<SocketVersion>();
  if (!other_fv) return false;

  // If one version is empty and the other is not, the writes cannot be coalesced
  if (_empty != other_fv->_empty) return false;

  // If the versions have different mtimes, the writes cannot be coalesced
  if (_mtime != other_fv->_mtime) return false;

  // If the versions have different hash values, the writes cannot be coalesced
  if (_hash != other_fv->_hash) return false;

  // Allow write coalescing
  return true;
}
*/