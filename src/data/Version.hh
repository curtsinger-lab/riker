#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include <sys/stat.h>
#include <sys/types.h>

#include "util/serializer.hh"

using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;
using std::weak_ptr;

class Command;
class Reference;

using Metadata = struct stat;
using Fingerprint = struct stat;

/// A reference to a specific version of an artifact
class Version : public std::enable_shared_from_this<Version> {
 public:
  Version(shared_ptr<Reference> ref, shared_ptr<Command> creator = nullptr) :
      _ref(ref), _creator(creator) {}

  virtual ~Version() = default;

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) = default;
  Version& operator=(Version&&) = default;

  /// Record a version that follows this one
  void followedBy(shared_ptr<Version> v);

  /// Get the first version of this artifact
  shared_ptr<Version> getFirstVersion();

  /// Get the number of preceding versions of this artifact
  size_t getVersionNumber() const { return _previous ? (_previous->getVersionNumber() + 1) : 0; }

  /// Get the previous version
  shared_ptr<Version> getPrevious() const { return _previous; }

  /// Get the next version
  shared_ptr<Version> getNext() const { return _next.lock(); }

  /// Get the reference used to establish this reference
  virtual shared_ptr<Reference> getReference() const { return _ref; }

  /// Get the path for this version if it has one
  virtual optional<string> getPath() const;

  /// Get the command that created this version
  shared_ptr<Command> getCreator() const { return _creator.lock(); }

  /// Is this version saved in a way that allows us to reproduce it?
  virtual bool isSaved() const { return false; }

  /// Do we have saved metadata for this version?
  virtual bool hasMetadata() const { return _metadata.has_value(); }

  /// Save the metadata for this version
  virtual void saveMetadata();

  /// Compare metadata for this version to another version
  virtual bool metadataMatch(shared_ptr<Version> other) const;

  /// Do we have a fingerprint for the contents of this version?
  virtual bool hasFingerprint() const { return false; }

  /// Save a fingerprint of this version's contents
  virtual void saveFingerprint() {}

  /// Compare the fingerprint for this version to another version
  virtual bool fingerprintMatch(shared_ptr<Version> other) const;

  /// Check if this version has been accessed
  bool isAccessed() const { return _accessed; }

  /// Mark this version as accessed
  void setAccessed() { _accessed = true; }

  /// Print a Version
  friend ostream& operator<<(ostream& o, const Version& v) {
    if (auto p = v.getPath(); p.has_value()) {
      return o << "[Artifact " << p.value() << "]@v" << v.getVersionNumber();
    } else {
      return o << "[Artifact]@v" << v.getVersionNumber();
    }
  }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const Version* v) { return o << *v; }

 protected:
  /// Get saved metadata for this version
  virtual optional<Metadata> getMetadata() const { return _metadata; }

  /// Get the saved fingerprint for this version
  virtual optional<Fingerprint> getFingerprint() const { return nullopt; }

  // Specify fields for serialization
  Version() = default;
  SERIALIZE(_previous, _next, _ref, _creator, _metadata);

 private:
  /// The version that preceded this one, if any
  shared_ptr<Version> _previous;

  /// The version that comes after this one, if any
  weak_ptr<Version> _next;

  /// The reference used to establish this version
  shared_ptr<Reference> _ref;

  /// The command that creatd this version
  weak_ptr<Command> _creator;

  /// Saved metadata for this version
  optional<Metadata> _metadata;

  /// Transient: has this version been accessed?
  bool _accessed = false;
};

class OpenedVersion : public Version {
 public:
  OpenedVersion(shared_ptr<Reference> ref) : Version(ref) {}

  virtual void saveFingerprint() override { saveMetadata(); }

 private:
  // Create default constructor and specify fields for serialization
  OpenedVersion() = default;
  SERIALIZE(cereal::base_class<Version>(this));
};
