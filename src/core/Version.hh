#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include <fcntl.h>

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
  friend class Artifact;

  /// Default constructor for deserialization
  Version() = default;

  /// Create a reference to a numbered version of an artifact
  Version(optional<string> path, shared_ptr<Command> creator = nullptr) :
      _path(path), _creator(creator) {}

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) = default;
  Version& operator=(Version&&) = default;

  /// Does this version have a known path?
  bool hasPath() const { return _path.has_value(); }

  /// Get the path for this version
  string getPath() const { return _path.value_or("anon"); }

  /// Check if this artifact corresponds to a system file
  bool isSystemFile() const;

  /// Get the first version of this artifact
  shared_ptr<Version> getFirstVersion();

  /// Get the previous version
  shared_ptr<Version> getPrevious() const { return _previous; }

  /// Get the next version
  shared_ptr<Version> getNext() const { return _next; }

  /// Get the index of this version
  size_t getIndex() const { return _index; }

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
    return o << "[" << v.getPath() << "]@" << v._index;
  }

  /// Print a version pointer
  friend ostream& operator<<(ostream& o, const Version* v) { return o << *v; }

 private:
  /// The path to this version, if we have one
  optional<string> _path;

  /// The version number of this artifact
  size_t _index;

  /// The version that preceded this one, if any
  shared_ptr<Version> _previous;

  /// The version that comes after this one, if any
  shared_ptr<Version> _next;

  weak_ptr<Command> _creator;              //< Which command created this version?
  optional<struct stat> _metadata;         //< Saved metadata for this version
  optional<vector<uint8_t>> _fingerprint;  //< Saved fingerprint for this version
  optional<string> _saved;                 //< Name of the file that contains a copy of this version

  /*** Transient Data (not serialized) ***/
  bool _accessed = false;  //< Has this version been accessed by any commands?
};
