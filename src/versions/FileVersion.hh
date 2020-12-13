#pragma once

#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <utility>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "blake3.h"
#include "string.h"
#include "ui/options.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::nullopt;
using std::optional;
using std::string;
using std::stringstream;

#define BLAKE3BUFSZ 65536 /* taken from BLAKE3 demo app without much thought! */

typedef std::array<uint8_t, BLAKE3_OUT_LEN> BLAKE3Hash;

class Ref;

struct FileFingerprint {
 public:
  bool empty;
  struct timespec mtime;
  std::optional<BLAKE3Hash> b3hash;

  /// Default constructor for deserialization
  FileFingerprint() noexcept = default;

  /// Create a fingerprint from stat data
  FileFingerprint(string path, struct stat& statbuf, int rc, fs::path cache_dir) noexcept {
    ASSERT(rc == 0) << "Cannot take fingerprint for nonexistent file '" << path << "'";

    // save mtime from statbuf
    empty = statbuf.st_size == 0;
    mtime = statbuf.st_mtim;

    // if file is readable and hashable and the user did not disable hashes, save hash
    if (rc == 0 && !options::mtime_only) {
      // is this a regular file?
      if (!(statbuf.st_mode & S_IFREG)) return;

      b3hash = blake3(path);
      // if caching is enabled, cache the file
      if (options::enable_cache) {
        cache(statbuf, b3hash.value(), path, cache_dir);
      }
    }
  }

  std::optional<BLAKE3Hash> blake3(fs::path path) {
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

  void cache(struct stat& statbuf, BLAKE3Hash& hash, fs::path path, fs::path cache_dir) noexcept {
    // We use a three-level directory prefix scheme to store cached files
    // to avoid having too many files in a given folder.  This scheme
    // below has 16^6 unique directory prefixes.
    string hash_str = b3hex(hash);
    fs::path dir_lvl_0 = hash_str.substr(0, 2);
    fs::path dir_lvl_1 = hash_str.substr(2, 2);
    fs::path dir_lvl_2 = hash_str.substr(4, 2);
    fs::path hash_dir = cache_dir / dir_lvl_0 / dir_lvl_1 / dir_lvl_2;

    // Path to cache file
    fs::path hash_file = hash_dir / hash_str;

    // Get the length of the input file
    loff_t len = statbuf.st_size;

    // Does the cache file already exist?  If so, skip. (reuse statbuf)
    bool file_exists(::lstat(hash_file.c_str(), &statbuf) == 0);
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

  /// Create a fingerprint for an empty file
  static FileFingerprint makeEmpty() noexcept {
    FileFingerprint f;
    f.empty = true;
    return f;
  }

  /// Compare to another fingerprint instance
  bool operator==(const FileFingerprint& other) const noexcept {
    // Two empty files are always equivalent
    if (empty && other.empty) {
      LOG(artifact) << "Not checking equality for fingerprint: both files are empty.";
      return true;
    }

    // Did the user disable fingerprinting?
    if (options::mtime_only) {
      // yes, compare mtimes
      bool mtime_is_same = std::tie(mtime.tv_sec, mtime.tv_nsec) ==
                           std::tie(other.mtime.tv_sec, other.mtime.tv_nsec);

      LOG(artifact) << "Checking equality for mtime-only fingerprint: mtime "
                    << (mtime_is_same ? "is same" : "is different");

      return mtime_is_same;
    } else {
      // no, compare hashes
      bool hash_is_same = b3hash.has_value() && other.b3hash.has_value()
                              ? b3hash.value() == other.b3hash.value()
                              : true;

      LOG(artifact) << "Checking equality for BLAKE3 fingerprint: hash "
                    << (hash_is_same ? "is same" : "is different");

      return hash_is_same;
    }
  }

  friend ostream& operator<<(ostream& o, const FileFingerprint& f) {
    if (f.empty) return o << "empty";
    return o << "mtime=" << f.mtime.tv_sec << "." << std::setfill('0') << std::setw(9)
             << f.mtime.tv_nsec << ", b3hash=" << f.b3hex();
  }

  string b3hex() const noexcept {
    if (!b3hash.has_value()) {
      return "[NO HASH]";
    }
    return b3hex(b3hash.value());
  }

  SERIALIZE(empty, mtime, b3hash);

 private:
  static string b3hex(BLAKE3Hash b3hash) noexcept {
    stringstream ss;
    for (int byte : b3hash) {
      ss << std::setfill('0') << std::setw(2) << std::hex << byte;
    }
    return ss.str();
  }
};

class FileVersion final : public Version {
 public:
  /// Create a FileVersion with no existing fingerprint
  FileVersion() noexcept = default;

  /// Create a FileVersion with an existing fingerprint
  FileVersion(FileFingerprint&& f) noexcept : _fingerprint(f) {}

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept override { return "content"; }

  /// Can this version be committed to the filesystem?
  bool canCommit() const noexcept;

  /// Commit this version to the filesystem
  void commit(fs::path path, mode_t mode = 0600) noexcept;

  /// Save a fingerprint of this version
  virtual void fingerprint(TraceHandler& handler,
                           fs::path path,
                           fs::path cache_dir) noexcept override;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    auto other_file = other->as<FileVersion>();
    if (!other_file) return false;
    if (other_file.get() == this) return true;
    return _fingerprint == other_file->_fingerprint;
  }

  virtual ostream& print(ostream& o) const noexcept override {
    if (_fingerprint.has_value()) {
      return o << "[file content: " << _fingerprint.value() << "]";
    } else {
      return o << "[file content: unknown]";
    }
  }

 private:
  optional<FileFingerprint> _fingerprint;

  SERIALIZE(BASE(Version), _fingerprint);
};
