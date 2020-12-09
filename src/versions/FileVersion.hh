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
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::nullopt;
using std::optional;
using std::string;
using std::stringstream;

#define BLAKE3BUFSZ 65536 /* taken from BLAKE3 demo app without much thought! */

class Ref;

struct FileFingerprint {
 public:
  bool empty;
  struct timespec mtime;
  std::optional<std::array<uint8_t, BLAKE3_OUT_LEN>> b3hash;

  /// Default constructor for deserialization
  FileFingerprint() noexcept = default;

  /// Create a fingerprint from stat data
  FileFingerprint(string path) noexcept {
    // Get stat data and save it
    struct stat statbuf;
    int rc = ::lstat(path.c_str(), &statbuf);

    // save mtime from statbuf
    empty = statbuf.st_size == 0;
    mtime = statbuf.st_mtim;

    // if file is readable and hashable, save hash
    if (rc == 0) b3hash = blake3(path);
  }

  std::optional<std::array<uint8_t, BLAKE3_OUT_LEN>> blake3(fs::path path) {
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
    std::array<uint8_t, BLAKE3_OUT_LEN> output;

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

  /// Create a fingerprint for an empty file
  static FileFingerprint makeEmpty() noexcept {
    FileFingerprint f;
    f.empty = true;
    return f;
  }

  /// Compare to another fingerprint instance
  bool operator==(const FileFingerprint& other) const noexcept {
    // Two empty files are always equivalent
    if (empty && other.empty) return true;

    // Otherwise compare mtimes
    bool mtime_is_same =
        std::tie(mtime.tv_sec, mtime.tv_nsec) == std::tie(other.mtime.tv_sec, other.mtime.tv_nsec);

    bool hash_is_same = b3hash.has_value() && other.b3hash.has_value()
                            ? b3hash.value() == other.b3hash.value()
                            : true;

    return mtime_is_same && hash_is_same;
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
    stringstream ss;
    for (int byte : b3hash.value()) {
      ss << std::setfill('0') << std::setw(2) << std::hex << byte;
    }
    return ss.str();
  }

  SERIALIZE(empty, mtime, b3hash);
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
  virtual void fingerprint(TraceHandler& handler, fs::path path) noexcept override;

  /// Check if this version has a fingerprint
  // virtual bool hasFingerprint() const noexcept override { return _fingerprint.has_value(); }

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
