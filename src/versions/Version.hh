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
class Version : public std::enable_shared_from_this<Version> {
 public:
  Version() noexcept = default;

  virtual ~Version() noexcept = default;

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) noexcept = default;
  Version& operator=(Version&&) noexcept = default;

  /// Try to cast this version to one of its subtypes
  template <class T>
  shared_ptr<T> as() noexcept {
    return std::dynamic_pointer_cast<T>(shared_from_this());
  }

  /// Get the command that created this version
  shared_ptr<Command> getCreator() const noexcept { return _creator.lock(); }

  /// Record that this version was created by command c
  void createdBy(shared_ptr<Command> c) noexcept { _creator = c; }

  /// Check if this version has been accessed
  bool isAccessed() const noexcept { return _accessed; }

  /// Record that this version has been accessed
  void accessed() noexcept { _accessed = true; }

  /// Get the name for the type of version this is
  virtual string getTypeName() const noexcept = 0;

  /// Is this version saved in a way that allows us to reproduce it?
  virtual bool isSaved() const noexcept = 0;

  /// Save this version
  virtual void save(shared_ptr<Reference> ref) noexcept = 0;

  /// Restore this version to the filesystem
  virtual void commit(shared_ptr<Reference> ref) const noexcept = 0;

  /// Is this version fingerprinted in a way that alllows us to check for a match?
  virtual bool hasFingerprint() const noexcept = 0;

  /// Fingerprint this version
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept = 0;

  /// Compare this version to another version
  virtual bool matches(shared_ptr<Version> other) const noexcept = 0;

  /// Print a Version
  friend ostream& operator<<(ostream& o, const Version& v) noexcept {
    if (v._identity.has_value()) {
      auto& [a, index] = v._identity.value();
      o << "[" << a->getTypeName() << " " << (a->getName().empty() ? "<anon>" : a->getName())
        << " v" << index;
    } else {
      o << "[Unknown Version";
    }
    if (v.isSaved()) {
      o << " (saved)";
    } else if (v.hasFingerprint()) {
      o << " (fingerprint)";
    }
    o << "]";
    return o;
  }

  /// Print a Version*
  friend ostream& operator<<(ostream& o, const Version* v) noexcept { return o << *v; }

 protected:
  SERIALIZE_EMPTY();

  /******** Transient Fields *********/

  /// The command that created this version
  weak_ptr<Command> _creator;

  /// Has this version been accessed by any command?
  bool _accessed = false;

  friend class Artifact;

  /// The artifact this version is attached to
  mutable optional<pair<const Artifact*, size_t>> _identity;

  /// Record a printable identity for this version that has just been attached to a given artifact
  void identify(const Artifact* a) const noexcept { _identity = {a, a->getVersionCount() - 1}; }

  /// Copy the identity to or from another version
  void identify(shared_ptr<Version> other) const noexcept {
    if (_identity.has_value()) {
      other->_identity = _identity;
    } else {
      _identity = other->_identity;
    }
  }
};
