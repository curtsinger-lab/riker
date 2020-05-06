#pragma once

#include <cstdint>
#include <list>
#include <memory>
#include <ostream>
#include <string>

#include "data/Version.hh"

class Command;

using std::list;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::to_string;

class Artifact {
 public:
  /****** Constructors ******/
  Artifact(string path) : _path(path) {}

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /// Get a reference to the latest version of this artifact
  shared_ptr<Version> getLatestVersion();

  /// Tag a new version of this artifact and return a reference to that version
  shared_ptr<Version> tagNewVersion(shared_ptr<Command> creator = nullptr);

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    o << "[Artifact " << a._path << "]";
    if (a._latest) {
      o << "@" << a._latest;
    }
    return o;
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) { return o << *a; }

 private:
  /// Friend method for serializing Artifacts
  template <class Archive>
  friend void serialize(Archive& archive, Artifact& a, const uint32_t version);

 private:
  string _path;                 //< The absolute, normalized path to this artifact
  shared_ptr<Version> _latest;  //< The latest version of this artifact
};
