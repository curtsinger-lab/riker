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

using std::nullopt;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;
using std::weak_ptr;

class Command;
class Reference;

/// A reference to a specific version of an artifact
class Version : public std::enable_shared_from_this<Version> {
 public:
  /// Create a reference to a numbered version of an artifact
  Version(optional<string> path = nullopt, shared_ptr<Command> creator = nullptr) :
      _path(path), _creator(creator) {}

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) = default;
  Version& operator=(Version&&) = default;

  /// Record a version that follows this one
  void followedBy(shared_ptr<Version> v);

  /// Does this version have a known path?
  bool hasPath() const { return _path.has_value() || (_previous && _previous->hasPath()); }

  /// Get the path for this version
  string getPath() const { return _path.value_or(_previous ? _previous->getPath() : "anon"); }

  /// Get the first version of this artifact
  shared_ptr<Version> getFirstVersion();

  /// Get the latest version of this artifact
  shared_ptr<Version> getLatestVersion();

  /// Get the number of preceding versions of this artifact
  size_t getVersionNumber() const { return _previous ? (_previous->getVersionNumber() + 1) : 0; }

  /// Get the previous version
  shared_ptr<Version> getPrevious() const { return _previous; }

  /// Get the next version
  shared_ptr<Version> getNext() const { return _next.lock(); }

  /// Get the command that created this version
  shared_ptr<Command> getCreator() const { return _creator.lock(); }

  /// Check if this version has been accessed
  bool isAccessed() const { return _accessed; }

  /// Mark this version as accessed
  void setAccessed() { _accessed = true; }

  /// Save the metadata for this version
  void saveMetadata(shared_ptr<Reference> ref);

  /// Check if this version has saved metadata
  bool hasMetadata() const { return _metadata.has_value(); }

  /// Get the saved metadata for this version
  const struct stat& getMetadata() const { return _metadata.value(); }

  /// Save a fingerprint of the contents for this version
  void saveFingerprint(shared_ptr<Reference> ref);

  /// Check if this version has a fingerprint saved
  bool hasFingerprint() const { return _fingerprint.has_value(); }

  /// Save the contents of this version of the artifact
  void saveContents(shared_ptr<Reference> ref);

  /// Check if the contents of this version have been saved
  bool hasSavedContents() const { return _saved.has_value(); }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Version& v, const uint32_t version);

  /// Print a version
  friend ostream& operator<<(ostream& o, const Version& v) {
    return o << "v" << v.getVersionNumber();
  }

  /// Print a version pointer
  friend ostream& operator<<(ostream& o, const Version* v) { return o << *v; }

 private:
  /// The path to this version, if we have one
  optional<string> _path;

  /// The version that preceded this one, if any
  shared_ptr<Version> _previous;

  /// The version that comes after this one, if any
  weak_ptr<Version> _next;

  /// Which command created this version?
  weak_ptr<Command> _creator;

  /// Saved metadata for this version
  optional<struct stat> _metadata;

  /// Saved fingerprint for this version
  optional<vector<uint8_t>> _fingerprint;

  /// Name of the file that contains a copy of this version
  optional<string> _saved;

  /*** Transient Data (not serialized) ***/
  /// Has this version been accessed by any commands other than the creator?
  bool _accessed = false;
};
