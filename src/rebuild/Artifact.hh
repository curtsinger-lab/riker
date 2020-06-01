#pragma once

#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <string>

class Command;
class Reference;
class Version;

using std::list;
using std::optional;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;

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
   * Create a new artifact.
   * \param ref A reference to this artifact used for pretty-printing
   */
  Artifact(shared_ptr<Reference> ref) : _ref(ref) {}

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /// Get the creator of the latest version of this artifact
  shared_ptr<Command> getCreator() const { return _creator; }

  /// Get the latest version of this artifact's metadata
  shared_ptr<Version> getMetadata() const { return _versions.back(); }

  /// Get the latest version of this artifact's contents
  shared_ptr<Version> getContents() const { return _versions.back(); }

  /// Get the number of versions of this artifact
  size_t getVersionCount() const { return _versions.size(); }

  /// Get the list of versions of this artifact
  const list<shared_ptr<Version>>& getVersions() const { return _versions; }

  /// Check if this artifact has no versions
  bool empty() const { return _versions.empty(); }

  /**
   * Create a version of this artifact to reflect what is currently on the filesystem
   */
  void createExistingVersion();

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
   * This method should be called during execution to record a new version.
   * \param c The command that has set the metadata for this artifact
   * \returns the version created by this operation, or nullptr if no new version is necessary.
   */
  shared_ptr<Version> setMetadata(shared_ptr<Command> c);

  /**
   * Command c sets the metadata for this artifact to an existing version.
   * This method is used to replay the effect of a SET_METADATA step when emulating a command.
   * \param c The command that sets the metadata for this artifact
   * \param v The new metadata version that should be appended to this artifact's version list
   */
  void setMetadata(shared_ptr<Command> c, shared_ptr<Version> v);

  /**
   * Command c sets the contents of this artifact.
   * This method should be called during execution to record a new version.
   * \param c The command that set the contents of this artifact
   * \returns the version created by this operation, or nullptr if no new version is necessary.
   */
  shared_ptr<Version> setContents(shared_ptr<Command> c);

  /**
   * Command c sets the contents of this artifact to an existing version.
   * This methos is used to replay the effect of a SET_CONTENTS step when emulating a command.
   * \param c The command that sets the contents of this artifact
   * \param v The new contents version that should be appended to this artifact's version list
   */
  void setContents(shared_ptr<Command> c, shared_ptr<Version> v);

  /**
   * Save the metadata for the latest version of this artifact
   * \param ref The reference to this artifact that should be used to access metadata
   */
  void saveMetadata(shared_ptr<Reference> ref);

  /**
   * Save a fingerprint of the contents of the latest version of this artifact
   * \param ref The reference to this artifact that should be used to access contents
   */
  void saveFingerprint(shared_ptr<Reference> ref);

  /**
   * Get the path to this artifact if it has one.
   * This is ONLY useful for pretty printing artifacts; the actual path(s) to this artifact can
   * change during a build. Those changes will not be reflected in the path returned here.
   */
  optional<string> getPath() const;

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    if (auto p = a.getPath(); p.has_value()) {
      return o << "[Artifact " << p.value() << "]@v" << a._versions.size() - 1;
    } else {
      return o << "[Artifact]@v" << a._versions.size() - 1;
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

  /// Which command created the latest version of this artifact?
  shared_ptr<Command> _creator;

  /// Has the latest version been accessed by a command other than its creator?
  bool _accessed = false;

  /// Keep track of commands that have accessed metadata for the latest version
  set<shared_ptr<Command>> _metadata_accesses;

  /// Keep track of commands that have accessed contents for the latest version
  set<shared_ptr<Command>> _content_accesses;
};
