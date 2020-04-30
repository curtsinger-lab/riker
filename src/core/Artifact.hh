#pragma once

#include <cstdint>
#include <cstdio>
#include <initializer_list>
#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>
#include <vector>

#include <fcntl.h>

#include <cereal/access.hpp>

#include "util/UniqueID.hh"

class Command;

using std::enable_shared_from_this;
using std::list;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::to_string;
using std::vector;
using std::weak_ptr;

class Artifact;

/// A reference to a specific version of an artifact
class Version {
 public:
  /// Default constructor for deserialization only
  Version() = default;

  /// Create a reference to a numbered version of an artifact
  Version(shared_ptr<Artifact> artifact, size_t index, shared_ptr<Command> creator = nullptr) :
      _artifact(artifact), _index(index), _creator(creator) {}

  // Disallow Copy
  Version(const Version&) = delete;
  Version& operator=(const Version&) = delete;

  // Allow Move
  Version(Version&&) = default;
  Version& operator=(Version&&) = default;

  /// Get the artifact this version references
  shared_ptr<Artifact> getArtifact() const { return _artifact; }

  /// Get the index of this version
  size_t getIndex() const { return _index; }

  /// Get the command that created this version
  shared_ptr<Command> getCreator() const { return _creator.lock(); }

  /// Check if this version has been accessed
  bool isAccessed() const { return _accessed; }

  /// Mark this version as accessed
  void setAccessed() { _accessed = true; }

  /// Save the metadata for this version
  void saveMetadata();

  /// Check if this version has saved metadata
  bool hasMetadata() const { return _metadata.has_value(); }

  /// Get the saved metadata for this version
  const struct stat& getMetadata() const { return _metadata.value(); }

  /// Save a fingerprint of the contents for this version
  void saveFingerprint();

  /// Check if this version has a fingerprint saved
  bool hasFingerprint() const { return _fingerprint.has_value(); }

  /// Save the contents of this version of the artifact
  void saveContents();

  /// Check if the contents of this version have been saved
  bool hasSavedContents() const { return _saved.has_value(); }

  /// Comparison function so versions can be used in sets and maps
  bool operator<(const Version& other) const {
    return std::tie(_artifact, _index) < std::tie(other._artifact, other._index);
  }

  /// Equality check for versions
  bool operator==(const Version& other) const {
    return std::tie(_artifact, _index) == std::tie(other._artifact, other._index);
  }

  /// Inequality check
  bool operator!=(const Version& other) const { return !(*this == other); }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, Version& v, const uint32_t version);

  /// Print a version
  friend ostream& operator<<(ostream& o, const Version& v) {
    return o << v._artifact << "@" << v._index;
    ;
  }

 private:
  //< The artifact this is a version of
  shared_ptr<Artifact> _artifact;

  // The version number of this artifact
  size_t _index;

  weak_ptr<Command> _creator;              //< Which command created this version?
  optional<struct stat> _metadata;         //< Saved metadata for this version
  optional<vector<uint8_t>> _fingerprint;  //< Saved fingerprint for this version
  optional<string> _saved;                 //< Name of the file that contains a copy of this version

  /*** Transient Data (not serialized) ***/
  bool _accessed = false;  //< Has this version been accessed by any commands?
};

class Artifact : public enable_shared_from_this<Artifact> {
 private:
  // Default constructor for deserialization
  friend class cereal::access;
  Artifact() = default;

 public:
  /****** Constructors ******/
  Artifact(string path) : _path(path) {}

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  // Forward declaration for VerionRef class, which stores a reference to a specific version of an
  // artifact.
  friend class Version;

  /// Get the unique ID assigned to this artifact
  size_t getID() const { return _id; }

  /// Get the path used to refer to this artifact
  const string& getPath() const { return _path; }

  /// Update the path used to refer to this artifact
  void updatePath(string path) { _path = path; }

  /// Get a short, printable name for this artifact
  string getShortName() const { return _path; }

  /// Return the number of versions we're tracking for this artifact
  size_t getVersionCount() const { return _versions.size(); }

  /// Check if this artifact corresponds to a system file
  bool isSystemFile() const;

  /// Get a reference to the latest version of this artifact
  shared_ptr<Version> getLatestVersion();

  /// Tag a new version of this artifact and return a reference to that version
  shared_ptr<Version> tagNewVersion(shared_ptr<Command> creator = nullptr);

  /// Get the list of versions of this artifact
  const vector<shared_ptr<Version>>& getVersions() const { return _versions; }

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& f) {
    if (!f.getPath().empty())
      return o << "[Artifact " << f.getPath() << "]";
    else
      return o << "[Artifact " << f.getID() << "]";
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* f) { return o << *f; }

 private:
  /// Friend method for serializing Artifacts
  template <class Archive>
  friend void serialize(Archive& archive, Artifact& a, const uint32_t version);

 private:
  UniqueID<Artifact> _id;                 //< A unique ID for printing
  string _path;                           //< The absolute, normalized path to this artifact
  vector<shared_ptr<Version>> _versions;  //< The sequence of versions of this artifact
};
