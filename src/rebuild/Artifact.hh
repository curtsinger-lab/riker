#pragma once

#include <memory>
#include <ostream>

#include "data/Version.hh"

class Command;

using std::ostream;
using std::shared_ptr;

/**
 * An artifact is a thin wrapper class around a sequence of artifact versions. The artifact
 * represents a single file, pipe, socket, etc. that is accessed and (potentially) modified
 * throughout its life. Artifact instances are not serialized, but are used during building to
 * ensure all operations on a given file, pipe, etc. refer to the latest versions of that artifact.
 */
class Artifact {
 private:
  /**
   * Create a new artifact that does not reference any version
   */
  Artifact() = default;

 public:
  /**
   * Create a new artifact from its initial version.
   * \param version The initial version of this artifact.
   */
  Artifact(shared_ptr<Version> version) : _version(version) {}

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /**
   * Check if an artifact resolves to at least one version
   * \returns true if there is a version for this artifact
   */
  operator bool() const { return _version != nullptr; }

  /**
   * Get the latest version of this artifact. This also advances the internal version pointer.
   * \returns A shared pointer to the latest version
   */
  shared_ptr<Version> get() const {
    if (!_version) return _version;

    auto next = _version->getNext();
    while (next) {
      _version = next;
      next = _version->getNext();
    }

    return _version;
  }

  /// Make Artifact instances behave like version pointers
  shared_ptr<Version> operator->() const { return get(); }

  /// Implicitly convert from Artifact to a Version pointer
  operator shared_ptr<Version>() const { return get(); }

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    // TODO: Print a canonical path when we have one
    return o << "[Artifact]@" << a->getVersionNumber();
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) { return o << *a; }

  /// Create a single artifact instance for empty artifacts
  static Artifact& getEmptyArtifact() {
    static Artifact _empty;
    return _empty;
  }

 private:
  /// Some version of this artifact. When accessed through the methods of this class, _version is
  /// always advanced to the newest version of the artifact.
  mutable shared_ptr<Version> _version;
};
