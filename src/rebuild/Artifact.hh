#pragma once

#include <cstdint>
#include <list>
#include <memory>
#include <ostream>
#include <string>

#include "data/Version.hh"

class Command;

using std::list;
using std::make_shared;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::to_string;

/**
 * An artifact is a thin wrapper class around a sequence of artifact versions. The artifact
 * represents a single file, pipe, socket, etc. that is accessed and (potentially) modified
 * throughout its life. Artifact instances are not serialized, but are used during building to
 * ensure all operations on a given file, pipe, etc. refer to the latest versions of that artifact.
 */
class Artifact {
 public:
  /**
   * Create a new artifact from its initial version.
   * \param version The initial version of this artifact.
   */
  Artifact(shared_ptr<Version> version) : _version(version) {}

  // Disallow copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /**
   * Access the latest version of this artifact. This access willadvance the Artifact's _version
   * pointer to refer to the latest version so far.
   * \returns A shared pointer to the latest version of this artifact
   */
  shared_ptr<Version> operator->() const {
    _version = _version->getLatestVersion();
    return _version;
  }

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    if (auto path = a->getPath(); path.has_value()) {
      return o << "[Artifact " << path.value() << "]@" << a->getVersionNumber();
    } else {
      return o << "[Artifact]@" << a->getVersionNumber();
    }
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) { return o << *a; }

 private:
  /// Some version of this artifact. When accessed through the methods of this class, _version is
  /// always advanced to the newest version of the artifact.
  mutable shared_ptr<Version> _version;
};
