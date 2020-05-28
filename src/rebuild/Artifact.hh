#pragma once

#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <set>

#include "data/IR.hh"
#include "data/Version.hh"

class Command;

using std::list;
using std::optional;
using std::ostream;
using std::set;
using std::shared_ptr;

/**
 * An artifact is a thin wrapper class around a sequence of artifact versions. The artifact
 * represents a single file, pipe, socket, etc. that is accessed and (potentially) modified
 * throughout its life. Artifact instances are not serialized, but are used during building to
 * ensure all operations on a given file, pipe, etc. refer to the latest versions of that
 * artifact.
 */
class Artifact {
 public:
  /**
   * Create a new artifact from its initial version.
   * \param version The initial version of this artifact.
   */
  Artifact(shared_ptr<Reference> ref) : _ref(ref) {}

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /// Get the latest version of this artifact
  shared_ptr<Version> getLatestVersion() const { return _versions.back(); }

  /// Advance this artifact to a new version
  void addVersion(shared_ptr<Version> v) { _versions.push_back(v); }

  /**
   * Command c accesses the metadata for this artifact.
   * Return the version c will observe, or nullptr if this version has already been accessed.
   */
  shared_ptr<Version> accessMetadata(shared_ptr<Command> c);

  /**
   * Command c accesses the contents of this artifact.
   * Return the version c will observe, or nullptr if this version has already been accessed.
   */
  shared_ptr<Version> accessContents(shared_ptr<Command> c);

  /**
   * Command c sets the metadata for this artifact.
   * Return the version created by this operation, or nullptr if no new version is necessary.
   */
  shared_ptr<Version> setMetadata(shared_ptr<Command> c);

  /**
   * Command c sets the contents of this artifact.
   * Return the version created by this operation, or nullptr if no new version is necessary.
   */
  shared_ptr<Version> setContents(shared_ptr<Command> c);

  /// Get the path to this artifact, if it has one.
  /// This is ONLY useful for pretty printing artifacts; the actual path(s) to this artifact can
  /// change during a build.
  optional<string> getPath() const;

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

 private:
  /// Tag a new version of this artifact created by command c (may be nullptr)
  shared_ptr<Version> tagNewVersion(shared_ptr<Command> c);

  /// Check if a command has already accessed the metadata for the latest version of this artifact
  bool metadataAccessedBy(shared_ptr<Command> c);

  /// Check if a command has already accessed the contents for the latest version of this artifact
  bool contentsAccessedBy(shared_ptr<Command> c);

  /// The reference used for the first access to this artifact
  shared_ptr<Reference> _ref;

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;

  /// Keep track of commands that have accessed metadata for the latest version
  set<shared_ptr<Command>> _metadata_accesses;

  /// Keep track of commands that have accessed contents for the latest version
  set<shared_ptr<Command>> _content_accesses;
};
