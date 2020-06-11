#pragma once

#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <utility>

#include "artifacts/Artifact.hh"
#include "util/serializer.hh"

using std::optional;
using std::ostream;
using std::pair;
using std::shared_ptr;
using std::string;

class Artifact;
class Reference;

/// A reference to a specific version of an artifact
class Version {
 public:
  Version() = default;

  virtual ~Version() = default;

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) = default;
  Version& operator=(Version&&) = default;

  /// Get the command that created this version
  shared_ptr<Command> getCreator() const { return _creator; }

  /// Record that this version was created by command c
  void createdBy(shared_ptr<Command> c) { _creator = c; }

  /// Check if this version has been accessed
  bool isAccessed() const { return _accessed; }

  /// Record that this version has been accessed
  void accessed() { _accessed = true; }

  /// Get the name for the type of version this is
  virtual string getTypeName() const = 0;

  /// Is this version saved in a way that allows us to reproduce it?
  virtual bool isSaved() const = 0;

  /// Save this version
  virtual void save(const shared_ptr<Reference>& ref) = 0;

  /// Restore this version to the filesystem
  virtual void commit(const shared_ptr<Reference>& ref) const = 0;

  /// Is this version fingerprinted in a way that alllows us to check for a match?
  virtual bool hasFingerprint() const = 0;

  /// Fingerprint this version
  virtual void fingerprint(const shared_ptr<Reference>& ref) = 0;

  /// Compare this version to another version
  virtual bool matches(const shared_ptr<Version>& other) const = 0;

  /// Print a Version
  friend ostream& operator<<(ostream& o, const Version& v) {
    if (v._identity.has_value()) {
      auto& [a, index] = v._identity.value();
      return o << "[" << (a->getName().empty() ? "<anon>" : a->getName()) << " v" << index << "]";
    } else {
      return o << "[Unknown Version]";
    }
  }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const Version* v) { return o << *v; }

 protected:
  SERIALIZE_EMPTY();

  /******** Transient Fields *********/

  /// The command that created this version
  shared_ptr<Command> _creator;

  /// Has this version been accessed by any command?
  bool _accessed = false;

  friend class Artifact;

  /// The artifact this version is attached to
  mutable optional<pair<const Artifact*, size_t>> _identity;

  /// Record a printable identity for this version that has just been attached to a given artifact
  void identify(const Artifact* a) const { _identity = {a, a->getVersionCount() - 1}; }

  /// Copy the identity to or from another version
  void identify(const shared_ptr<Version>& other) const {
    if (_identity.has_value()) {
      other->_identity = _identity;
    } else {
      _identity = other->_identity;
    }
  }
};
