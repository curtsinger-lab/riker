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

struct ContentFingerprint {
 public:
  bool empty;
  struct timespec mtime;

  /// Default constructor for deserialization
  ContentFingerprint() noexcept = default;

  /// Create a fingerprint from stat data
  ContentFingerprint(struct stat& s) noexcept : empty(s.st_size == 0), mtime(s.st_mtim) {}

  /// Create a fingerprint for an empty file
  static ContentFingerprint makeEmpty() noexcept {
    ContentFingerprint f;
    f.empty = true;
    return f;
  }

  /// Compare to another fingerprint instance
  bool operator==(const ContentFingerprint& other) const noexcept {
    // Two empty files are always equivalent
    if (empty && other.empty) return true;

    // Otherwise compare mtimes
    return std::tie(mtime.tv_sec, mtime.tv_nsec) ==
           std::tie(other.mtime.tv_sec, other.mtime.tv_nsec);
  }

  friend ostream& operator<<(ostream& o, const ContentFingerprint& f) {
    if (f.empty) return o << "empty file";
    return o << "mtime=" << f.mtime.tv_sec << "." << std::setfill('0') << std::setw(9)
             << f.mtime.tv_nsec;
  }

  SERIALIZE(empty, mtime);
};

class ContentVersion final : public Version {
 public:
  /// Create a ContentVersion with no existing fingerprint
  ContentVersion() noexcept = default;

  /// Create a ContentVersion with an existing fingerprint
  ContentVersion(ContentFingerprint&& f) noexcept : _fingerprint(f) {}

  /// Get the name for this type of version
  virtual string getTypeName() const noexcept override { return "content"; }

  /// Is this version saved in a way that can be committed?
  virtual bool isSaved() const noexcept override;

  /// Save this version so it can be committed later
  virtual void save(shared_ptr<Reference> ref) noexcept override {}

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> ref) const noexcept override;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  virtual bool hasFingerprint() const noexcept override { return _fingerprint.has_value(); }

  /// Save a fingerprint of this version
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept override;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept override;

  virtual ostream& print(ostream& o) const noexcept override {
    if (_fingerprint.has_value()) {
      return o << "[file content: " << _fingerprint.value() << "]";
    } else {
      return o << "[file content: unknown]";
    }
  }

 private:
  optional<ContentFingerprint> _fingerprint;

  SERIALIZE(BASE(Version), _fingerprint);
};
