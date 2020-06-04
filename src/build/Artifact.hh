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
  Artifact(Build* build, string name) : _build(build), _name(name) {}

  void createInitialVersion(shared_ptr<Command> creator);

 public:
  static shared_ptr<Artifact> existing(Build* build, string name, shared_ptr<Reference> ref);

  static shared_ptr<Artifact> created(Build* build, string name, shared_ptr<Reference> ref,
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

  /// Command c accesses the metadata for this artifact using reference ref.
  /// Return the version command c should check for, or nullptr if no check is necessary.
  shared_ptr<Version> accessMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /// Command c accesses the contents of this artifact using reference ref.
  /// Return the version command c should check for, or nullptr if no check is necessary.
  shared_ptr<Version> accessContents(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /// Command c sets this artifact's metadata using reference ref.
  /// Return the version this creates, or nullptr if no new version is necessary.
  shared_ptr<Version> setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /// Command c sets this artifact's contents using reference ref.
  /// Return the version this creates, or nullptr if no new version is necessary.
  shared_ptr<Version> setContents(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /****** Emulation Methods ******/
  // These methods are called when emulated commands make accesses to this artifact

  /// Command c expects to see metadata matching version v
  void checkMetadata(shared_ptr<Command> c, shared_ptr<Version> v);

  /// Command c expects to see contents matching version v
  void checkContents(shared_ptr<Command> c, shared_ptr<Version> v);

  /// Command c sets the metadata for this artifact to version v
  void setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref, shared_ptr<Version> v);

  /// Command c sets the contents of this artifact to version v
  void setContents(shared_ptr<Command> c, shared_ptr<Reference> ref, shared_ptr<Version> v);

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
   * Get the name of this artifact used for pretty-printing
   */
  const string& getName() const { return _name; }

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    return o << "[Artifact " << a.getName() << "]@v" << a._versions.size() - 1;
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) { return o << *a; }

 private:
  /// The build that this artifact is part of
  Build* _build;

  /// The name of this artifact used for pretty-printing
  string _name;

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;

  shared_ptr<Version> _metadata_version;  //< The latest metadata version
  shared_ptr<Command> _metadata_creator;  //< The command that last changed this artifact's metadata
  shared_ptr<Reference> _metadata_ref;    //< The reference that was last used to change metadata
  bool _metadata_accessed = false;        //< Has this artifact's metadata been accessed?

  shared_ptr<Version> _content_version;  //< The latest content version
  shared_ptr<Command> _content_creator;  //< The command that last changed this artifact's content
  shared_ptr<Reference> _content_ref;    //< The reference that was last used to change content
  bool _content_accessed = false;        //< Has this artifact's content been accessed?
};
