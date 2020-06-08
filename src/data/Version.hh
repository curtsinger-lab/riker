#pragma once

#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "artifact/Artifact.hh"
#include "data/Metadata.hh"
#include "data/serializer.hh"

using std::optional;
using std::ostream;
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

  /// Get the name for the type of version this is
  virtual string getTypeName() const = 0;

  /// Is this version saved in a way that allows us to reproduce it?
  virtual bool isSaved() const = 0;

  /// Save this version
  virtual void save(shared_ptr<Reference> ref) = 0;

  /// Restore this version to the filesystem
  virtual void commit(shared_ptr<Reference> ref) const = 0;

  /// Is this version fingerprinted in a way that alllows us to check for a match?
  virtual bool isFingerprinted() const = 0;

  /// Fingerprint this version
  virtual void fingerprint(shared_ptr<Reference> ref) = 0;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const = 0;

  /// Print a Version
  friend ostream& operator<<(ostream& o, const Version& v) {
    return o << v._identity.value_or("[Unknown Version]");
  }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const Version* v) { return o << *v; }

 private:
  SERIALIZE_EMPTY();

  /******** Transient Fields *********/

  friend class Artifact;

  /// A printable identity for this version
  mutable optional<string> _identity;

  /// Record a printable identity for this version that has just been attached to a given artifact
  void identify(const Artifact* a) const {
    _identity = string("[") + a->getName() + " v" + std::to_string(a->getVersionCount() - 1) + "]";
  }

  /// Copy the identity to or from another version
  void identify(shared_ptr<Version> other) const {
    if (_identity.has_value()) {
      other->_identity = _identity;
    } else {
      _identity = other->_identity;
    }
  }
};
