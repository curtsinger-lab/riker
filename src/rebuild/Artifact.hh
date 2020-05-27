#pragma once

#include <list>
#include <memory>
#include <optional>
#include <ostream>

#include "data/IR.hh"
#include "data/Version.hh"

class Command;

using std::list;
using std::optional;
using std::ostream;
using std::shared_ptr;

/**
 * An artifact is a thin wrapper class around a sequence of artifact versions. The artifact
 * represents a single file, pipe, socket, etc. that is accessed and (potentially) modified
 * throughout its life. Artifact instances are not serialized, but are used during building to
 * ensure all operations on a given file, pipe, etc. refer to the latest versions of that
 * artifact.
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
  Artifact(shared_ptr<Reference> ref, shared_ptr<Version> version) : _ref(ref) {
    _versions.push_back(version);
  }

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /**
   * Move this artifact to a new version
   */
  void addVersion(shared_ptr<Version> v) { _versions.push_back(v); }

  /**
   * Check if an artifact resolves to at least one version
   * \returns true if there is a version for this artifact
   */
  operator bool() const { return !_versions.empty(); }

  /**
   * Get the latest version of this artifact.
   * \returns A shared pointer to the latest version
   */
  shared_ptr<Version> get() const { return _versions.back(); }

  /// Make Artifact instances behave like version pointers
  shared_ptr<Version> operator->() const { return get(); }

  /// Implicitly convert from Artifact to a Version pointer
  operator shared_ptr<Version>() const { return get(); }

  /// Get the path to this artifact, if it has one.
  /// This is ONLY useful for pretty printing artifacts; the actual path(s) to this artifact can
  /// change during a build.
  optional<string> getPath() const {
    if (auto a = dynamic_pointer_cast<Access>(_ref)) {
      return a->getPath();
    } else {
      return nullopt;
    }
  }

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    if (auto p = a.getPath(); p.has_value()) {
      return o << "[Artifact " << p.value() << "]@v" << a._versions.size();
    } else {
      return o << "[Artifact]@v" << a._versions.size();
    }
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) { return o << *a; }

  /// Create a single artifact instance for empty artifacts
  static Artifact& getEmptyArtifact() {
    static Artifact _empty;
    return _empty;
  }

 private:
  /// The reference used for the first access to this artifact
  shared_ptr<Reference> _ref;

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;
};
