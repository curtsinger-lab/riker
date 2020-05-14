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
   * Access the latest version of this artifact. This non-const implementation will advance the
   * Artifact's _version pointer to refer to the latest version so far.
   * \returns A shared pointer to the latest version of this artifact
   */
  shared_ptr<Version> operator->() {
    _version = _version->getLatestVersion();
    return _version;
  }

  /**
   * Access the latest version of this artifact. This const implementation does not advance the
   * Artifact's _version pointer, so repeated calls will traverse the linked list of versions
   * multiple times.
   * \returns A shared pointer to the latest version of this artifact
   */
  shared_ptr<Version> operator->() const { return _version->getLatestVersion(); }

  /// Tag a new version of this artifact and return a reference to that version
  shared_ptr<Version> tagNewVersion(shared_ptr<Command> creator = nullptr) {
    auto v = make_shared<Version>(nullopt, creator);
    _version = _version->getLatestVersion();
    _version->followedBy(v);
    _version = v;
    return v;
  }

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    if (a->hasPath()) {
      return o << "[Artifact " << a->getPath() << "]@" << a->getVersionNumber();
    } else {
      return o << "[Artifact]@" << a->getVersionNumber();
    }
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) { return o << *a; }

 private:
  /// A version of this artifact. This pointer will be advanced to reference the latest version when
  /// accessed through getLatestVersion(), but is not guaranteed to point to the latest version at
  /// all times
  shared_ptr<Version> _version;
};
