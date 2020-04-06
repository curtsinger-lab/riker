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

#include "ui/options.hh"

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

class Artifact : public enable_shared_from_this<Artifact> {
 public:
  // Forward declaration for VerionRef class, which stores a reference to a specific version of an
  // artifact.
  friend class VersionRef;

  // Default constructor for deserialization
  friend class cereal::access;
  Artifact() : _id(getNextID()) {}

  /****** Constructors ******/
  Artifact(string path) : _id(getNextID()), _path(path) {}

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /****** Getters and setters ******/

  /// Get the unique ID assigned to this artifact
  size_t getID() const { return _id; }

  /// Get the path used to refer to this artifact
  const string& getPath() const { return _path; }

  /// Update the path used to refer to this artifact
  void updatePath(string path) { _path = path; }

  /// Get a short, printable name for this artifact
  string getShortName() const { return _path; }

  /// Check if this artifact corresponds to a system file
  bool isSystemFile() {
    for (auto p : {"/usr/", "/lib/", "/etc/", "/dev/", "/proc/", "/bin/"}) {
      // Check if the path begins with one of our prefixes.
      // Using rfind with a starting index of 0 is equivalent to starts_with (coming in C++20)
      if (_path.rfind(p, 0) != string::npos) return true;
    }
    return false;
  }

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& f) {
    if (f.getPath() != "")
      return o << "[Artifact " << f.getPath() << "]";
    else
      return o << "[Artifact " << f.getID() << "]";
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* f) { return o << *f; }

  /// A reference to a specific version of this artifact
  class VersionRef {
    friend class Artifact;

   private:
    VersionRef(shared_ptr<Artifact> artifact, size_t index) : _artifact(artifact), _index(index) {}

   public:
    // Default constructor for deserialization only
    VersionRef() = default;

    shared_ptr<Artifact> getArtifact() const { return _artifact; }
    size_t getIndex() const { return _index; }
    string getShortName() const { return _artifact->getShortName() + "v" + to_string(_index); }

    bool operator<(const VersionRef& other) const {
      return std::tie(_artifact, _index) < std::tie(other._artifact, other._index);
    }

    /// Friend method for serialization
    template <class Archive>
    friend void serialize(Archive& archive, VersionRef& v);

   private:
    shared_ptr<Artifact> _artifact;
    size_t _index;
  };

  /// Tag a new version of this artifact and return a reference to that version
  VersionRef tagNewVersion() {
    _versions.push_back(Artifact::Version());
    return VersionRef(shared_from_this(), _versions.size() - 1);
  }

  /// Get a reference to the latest version of this artifact
  VersionRef getLatestVersion() {
    // Tag an initial version of each artifact on demand
    if (_versions.size() == 0) tagNewVersion();
    return VersionRef(shared_from_this(), _versions.size() - 1);
  }

  /// Construct a list of references to the versions of this artifact. This isn't particularly
  /// efficient, but it's only used in the GraphViz output.
  const list<VersionRef> getVersions() {
    list<VersionRef> result;
    for (size_t i = 0; i < _versions.size(); i++) {
      result.push_back(VersionRef(shared_from_this(), i));
    }
    return result;
  }

  friend ostream& operator<<(ostream& o, const Artifact::VersionRef& v) {
    return o << v.getArtifact() << "@" << v.getIndex();
  }

 private:
  /// Data about a specific version of this artifact. This struct is hidden from outside users.
  /// Outside code should use Artifact::VersionRef to refer to a specific version of an artifact.
  struct Version {
    bool has_metadata = false;  //< Do we have file metadata for this version?
    struct stat metadata;       //< If we have it, the metadata is stored here

    bool has_fingerprint = false;  //< Do we have a fingerprint of the file contents?
    vector<uint8_t> fingerprint;   //< The fingerprint
  };

  /// Friend method for serializing Artifacts
  template <class Archive>
  friend void serialize(Archive& archive, Artifact& a);

  /// Friend method for serializing Artifact::Versions
  template <class Archive>
  friend void serialize(Archive& archive, Artifact::Version& v);

 private:
  /// Fingerprint this artifact and save the fingerprint with the latest version of the artifact
  void fingerprint();

 private:
  size_t _id;                 //< A unique ID for printing
  string _path;               //< The absolute, normalized path to this artifact
  vector<Version> _versions;  //< The sequence of versions of this artifact

  /// Generate a unique ID for an artifact
  static size_t getNextID() {
    static size_t next_id = 0;
    return next_id++;
  }
};
