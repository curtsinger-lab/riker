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

  /// Compare to another fingerprint instance
  bool operator==(const ContentFingerprint& other) const noexcept {
    return std::tie(empty, mtime.tv_sec, mtime.tv_nsec) ==
           std::tie(other.empty, other.mtime.tv_sec, other.mtime.tv_nsec);
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
  virtual string getTypeName() const noexcept final { return "content"; }

  /// Is this version saved in a way that can be committed?
  virtual bool isSaved() const noexcept final;

  /// Save this version so it can be committed later
  virtual void save(shared_ptr<Reference> ref) noexcept final {}

  /// Commit this version to the filesystem
  virtual void commit(shared_ptr<Reference> ref) const noexcept final;

  /// Is this version fingerprinted in a way that allows us to check for a match?
  virtual bool hasFingerprint() const noexcept final { return _fingerprint.has_value(); }

  /// Save a fingerprint of this version
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept final;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept final;

 private:
  optional<ContentFingerprint> _fingerprint;

  SERIALIZE(BASE(Version), _fingerprint);
};
