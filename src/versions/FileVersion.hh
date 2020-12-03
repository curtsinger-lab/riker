#pragma once

#include <memory>
#include <optional>
#include <string>
#include <utility>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "blake3.h"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::optional;
using std::shared_ptr;
using std::string;

class Ref;

struct FileFingerprint {
 public:
  bool empty;
  struct timespec mtime;
  std::array<uint8_t, BLAKE3_OUT_LEN> b3hash;

  /// Default constructor for deserialization
  FileFingerprint() noexcept = default;

  /// Create a fingerprint from stat data
  FileFingerprint(struct stat& s) noexcept : empty(s.st_size == 0), mtime(s.st_mtim) {}

  /// Create a fingerprint from stat data and a BLAKE3 hash
  FileFingerprint(struct stat& s, std::array<uint8_t, BLAKE3_OUT_LEN>& hash) noexcept :
      empty(s.st_size == 0), mtime(s.st_mtim), b3hash(hash) {}

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
    return std::tie(mtime.tv_sec, mtime.tv_nsec, b3hash) ==
           std::tie(other.mtime.tv_sec, other.mtime.tv_nsec, other.b3hash);
  }

  friend ostream& operator<<(ostream& o, const FileFingerprint& f) {
    if (f.empty) return o << "empty";
    return o << "mtime=" << f.mtime.tv_sec << "." << std::setfill('0') << std::setw(9)
             << f.mtime.tv_nsec << ", b3hash=" << f.b3hex();
  }

  string b3hex() const noexcept {
    static const char hexchars[] = "01234567ABCDEF";
    string s;
    for (uint8_t byte : b3hash) {
      s.push_back(hexchars[byte >> 4]);   // extract upper nibble & lookup char
      s.push_back(hexchars[byte & 0xF]);  // extract lower nibble & lookup char
    }
    return s;
  }

  SERIALIZE(empty, mtime);
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
  virtual bool hasFingerprint() const noexcept override { return _fingerprint.has_value(); }

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
