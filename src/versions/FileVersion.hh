#pragma once

#include <memory>
#include <optional>
#include <string>
#include <utility>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "util/serializer.hh"
#include "versions/Version.hh"

using std::optional;
using std::shared_ptr;
using std::string;

class Reference;

struct FileFingerprint {
 public:
  bool empty;
  struct timespec mtime;

  /// Default constructor for deserialization
  FileFingerprint() noexcept = default;

  /// Create a fingerprint from stat data
  FileFingerprint(struct stat& s) noexcept : empty(s.st_size == 0), mtime(s.st_mtim) {}

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
    return std::tie(mtime.tv_sec, mtime.tv_nsec) ==
           std::tie(other.mtime.tv_sec, other.mtime.tv_nsec);
  }

  friend ostream& operator<<(ostream& o, const FileFingerprint& f) {
    if (f.empty) return o << "empty";
    return o << "mtime=" << f.mtime.tv_sec << "." << std::setfill('0') << std::setw(9)
             << f.mtime.tv_nsec;
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
  virtual bool canCommit() const noexcept override;

  /// Commit this version to the filesystem
  virtual void commit(fs::path path) noexcept override;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  bool hasFingerprint() const noexcept { return _fingerprint.has_value(); }

  /// Save a fingerprint of this version
  void fingerprint(fs::path path) noexcept;

  /// Compare this version to another version
  bool matches(shared_ptr<FileVersion> other) const noexcept {
    if (other.get() == this) return true;
    return _fingerprint == other->_fingerprint;
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
