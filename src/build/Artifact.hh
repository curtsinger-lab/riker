#pragma once

#include <functional>
#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <string>

using std::list;
using std::optional;
using std::ostream;
using std::reference_wrapper;
using std::set;
using std::shared_ptr;
using std::string;

class Build;
class Command;
class Env;
class Reference;
class Version;

/**
 * An artifact is a thin wrapper class around a sequence of artifact versions. The artifact
 * represents a single file, pipe, socket, etc. that is accessed and (potentially) modified
 * throughout its life. Artifact instances are not serialized, but are used during building to
 * ensure all operations on a given file, pipe, etc. refer to the latest versions of that
 * artifact.
 */
class Artifact : public std::enable_shared_from_this<Artifact> {
 private:
  /**
   * Create a new artifact. Only accessibly to this class and Env
   * \param build This artifact is instantiated as part of this build instance
   * \param ref   A reference to this artifact used for pretty-printing
   */
  Artifact(Build* build, shared_ptr<Reference> ref) : _build(build), _ref(ref) {}

  void createInitialVersion(shared_ptr<Command> creator);

 public:
  static shared_ptr<Artifact> existing(Build* build, shared_ptr<Reference> ref);

  static shared_ptr<Artifact> created(Build* build, shared_ptr<Reference> ref,
                                      shared_ptr<Command> c);

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  // Allow Move
  Artifact(Artifact&&) = default;
  Artifact& operator=(Artifact&&) = default;

  /// Get the creator of the latest version of this artifact
  shared_ptr<Command> getMetadataCreator() const { return _metadata_creator; }

  /// Get the creator of the latest version of this artifact
  shared_ptr<Command> getContentCreator() const { return _content_creator; }

  /// Get the number of versions of this artifact
  size_t getVersionCount() const { return _versions.size(); }

  /// Get the list of versions of this artifact
  const list<shared_ptr<Version>>& getVersions() const { return _versions; }

  /****** Tracing Methods *******/
  // These methods are called when executing commands make accesses to this artifact

  /// Command c accesses the metadata for this artifact. Return the version it sees.
  shared_ptr<Version> accessMetadata(shared_ptr<Command> c);

  /// Command c access the contents of this artifact. Return the version it sees.
  shared_ptr<Version> accessContents(shared_ptr<Command> c);

  /// Command c sets this artifact's metadata. Return the version this creates.
  shared_ptr<Version> setMetadata(shared_ptr<Command> c);

  /// Command c sets this artifact's contents. Return the version this creates.
  shared_ptr<Version> setContents(shared_ptr<Command> c);

  /****** Emulation Methods ******/
  // These methods are called when emulated commands make accesses to this artifact

  /// Command c expects to see metadata matching version v
  void checkMetadata(shared_ptr<Command> c, shared_ptr<Version> v);

  /// Command c expects to see contents matching version v
  void checkContents(shared_ptr<Command> c, shared_ptr<Version> v);

  /// Command c sets the metadata for this artifact to version v
  void setMetadata(shared_ptr<Command> c, shared_ptr<Version> v);

  /// Command c sets the contents of this artifact to version v
  void setContents(shared_ptr<Command> c, shared_ptr<Version> v);

  /****** Utility Methods ******/

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
   * Do we have sufficient saved data to commit this artifact to the filesystem?
   */
  bool isSaved() const;

  /**
   * Check this artifact's final state against the filesystem.
   * Report any change in content or metadata to the build.
   */
  void checkFinalState(shared_ptr<Reference> ref);

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
  /// The build that this artifact is part of
  Build* _build;

  /// The reference used for the first access to this artifact
  shared_ptr<Reference> _ref;

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;

  /// What is the latest metadata version?
  shared_ptr<Version> _metadata_version;

  /// What is the latest content version?
  shared_ptr<Version> _content_version;

  /// Which command wrote the latest metadata for this artifact?
  shared_ptr<Command> _metadata_creator;

  /// Which command wrote the latest contnet of this artifact?
  shared_ptr<Command> _content_creator;
};
