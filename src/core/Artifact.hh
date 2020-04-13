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
class ArtifactVersion {
 public:
  /// Default constructor for deserialization only
  ArtifactVersion() = default;

  /// Create a reference to a numbered version of an artifact
  ArtifactVersion(shared_ptr<Artifact> artifact, size_t index) :
      _artifact(artifact), _index(index) {}

  /// Get the artifact this version references
  shared_ptr<Artifact> getArtifact() const { return _artifact; }

  /// Get the index of this version
  size_t getIndex() const { return _index; }

  /// Get the command that created this version
  shared_ptr<Command> getCreator() const;

  /// Check if this version has been accessed
  bool isAccessed() const;

  /// Mark this version as accessed
  void setAccessed();

  /// Save the metadata for this version
  void saveMetadata();

  /// Check if this version has saved metadata
  bool hasMetadata() const;

  /// Save a fingerprint of the contents for this version
  void saveFingerprint();

  /// Check if this version has a fingerprint saved
  bool hasFingerprint() const;

  /// Save the contents of this version of the artifact
  void saveContents();

  /// Check if the contents of this version have been saved
  bool hasSavedContents() const;

  /// Comparison function so versions can be used in sets and maps
  bool operator<(const ArtifactVersion& other) const {
    return std::tie(_artifact, _index) < std::tie(other._artifact, other._index);
  }

  /// Friend method for serialization
  template <class Archive>
  friend void serialize(Archive& archive, ArtifactVersion& v, const uint32_t version);

  /// Print a version
  friend ostream& operator<<(ostream& o, const ArtifactVersion& v) {
    return o << v.getArtifact() << "@" << v.getIndex();
  }

 private:
  shared_ptr<Artifact> _artifact;  //< The artifact this is a version of
  size_t _index;  //< The index into the artifact's _versions list for this version's data
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
  friend class ArtifactVersion;

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
  ArtifactVersion getLatestVersion();

  /// Tag a new version of this artifact and return a reference to that version
  ArtifactVersion tagNewVersion(shared_ptr<Command> creator = nullptr);

  /// Construct a list of references to the versions of this artifact. This isn't particularly
  /// efficient, but it's only used in the GraphViz output.
  list<ArtifactVersion> getVersions();

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& f) {
    if (!f.getPath().empty())
      return o << "[Artifact " << f.getPath() << "]";
    else
      return o << "[Artifact " << f.getID() << "]";
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* f) { return o << *f; }

  /// Data about a specific version of this artifact. This struct is hidden from outside users.
  /// Outside code should use ArtifactVersion to refer to a specific version of an artifact.
  struct VersionData {
    /// Create a version with no recorded creator
    VersionData() {}

    /// Create a version with a given creator
    VersionData(shared_ptr<Command> c) : creator(c) {}

    weak_ptr<Command> creator;              //< Which command created this version?
    bool accessed = false;                  //< Has this version been accessed by any commands?
    optional<struct stat> metadata;         //< Saved metadata for this version
    optional<vector<uint8_t>> fingerprint;  //< Saved fingerprint for this version
    optional<string> saved;  //< Name of the file that contains a copy of this version
  };

 private:
  /// Friend method for serializing Artifacts
  template <class Archive>
  friend void serialize(Archive& archive, Artifact& a, const uint32_t version);

  /// Friend method for serializing Artifact::Versions
  template <class Archive>
  friend void serialize(Archive& archive, Artifact::VersionData& v, const uint32_t version);

 private:
  UniqueID<Artifact> _id;         //< A unique ID for printing
  string _path;                   //< The absolute, normalized path to this artifact
  vector<VersionData> _versions;  //< The sequence of versions of this artifact
};
