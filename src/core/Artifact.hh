#pragma once

#include <cstdint>
#include <list>
#include <memory>
#include <ostream>
#include <string>

#include <cereal/access.hpp>

class Command;
class Version;

using std::list;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::to_string;

class Artifact {
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

  /// Get a short, printable name for this artifact
  string getShortName() const { return _path; }

  /// Get a reference to the latest version of this artifact
  shared_ptr<Version> getLatestVersion();

  /// Tag a new version of this artifact and return a reference to that version
  shared_ptr<Version> tagNewVersion(shared_ptr<Command> creator = nullptr);

  /// Get the list of versions of this artifact
  list<shared_ptr<Version>> getVersions() const;

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& f) { return o << "[Artifact]"; }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* f) { return o << *f; }

 private:
  /// Friend method for serializing Artifacts
  template <class Archive>
  friend void serialize(Archive& archive, Artifact& a, const uint32_t version);

 private:
  string _path;                 //< The absolute, normalized path to this artifact
  shared_ptr<Version> _latest;  //< The latest version of this artifact
};
