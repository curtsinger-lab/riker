#pragma once

#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

using std::list;
using std::make_shared;
using std::ostream;
using std::shared_ptr;
using std::string;

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
 public:
  /**
   * Create a new artifact. Only accessibly to this class and Env
   * \param env       This artifact is instantiated as part of this environment
   * \param name      A name for this artifact used in pretty-printing
   * \param committed Does the initial version of this artifact represent the filesystem state?
   * \param v         An initial version the new artifact should be seeded with
   */
  Artifact(Env& env, string name, bool committed, shared_ptr<Version> v = make_shared<Version>());

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  /// Get the creator of the latest version of this artifact
  shared_ptr<Command> getMetadataCreator() const { return _metadata_creator; }

  /// Get the creator of the latest version of this artifact
  shared_ptr<Command> getContentCreator() const { return _content_creator; }

  /// Get the number of versions of this artifact
  size_t getVersionCount() const { return _versions.size(); }

  /// Get the list of versions of this artifact
  const list<shared_ptr<Version>>& getVersions() const { return _versions; }

  /**
   * Command c accesses the metadata for this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  shared_ptr<Version> accessMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /**
   * Command c accesses the content of this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  shared_ptr<Version> accessContents(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /**
   * Command c sets the metadata of this artifact using reference ref.
   * \param c         The command making the change
   * \param ref       The reference used to reach this artifact
   * \param committed Was the metadata change performed on the filesystem?
   * \returns the version the command creates, or nullptr if no new version is necessary
   */
  shared_ptr<Version> setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref, bool committed);

  /**
   * Command c sets the metadata of this artifact to version v using reference ref.
   * \param c         The command making the change
   * \param ref       The reference used to reach this artifact
   * \param v         The version this artifact's metadata is set to
   * \param committed Was the metadata change already performed on the filesystem?
   * \returns the newly-assigned metadata version
   */
  shared_ptr<Version> setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                  shared_ptr<Version> v, bool committed);

  /**
   * Command c sets the content of this artifact using reference ref.
   * \param c         The command making the change
   * \param ref       The reference used to reach this artifact
   * \param committed Was the content change already performed on the filesystem?
   * \returns the version the command creates, or nullptr if no new version is necessary
   */
  shared_ptr<Version> setContents(shared_ptr<Command> c, shared_ptr<Reference> ref, bool committed);

  /**
   * Command c sets the content of this artifact to version v using reference ref.
   * \param c         The command making the change
   * \param ref       The reference used to reach this artifact
   * \param v         The version this artifact's content is set to
   * \param committed Was the metadata change already performed on the filesystem?
   * \returns the newly-assigned content version
   */
  shared_ptr<Version> setContents(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                  shared_ptr<Version> v, bool committed);

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
   * Have all modifications to this artifact been committed to the filesystem?
   */
  bool isCommitted() const { return _committed_versions == _versions.size(); }

  /**
   * Commit any un-committed version of this artifact using the provided reference
   */
  void commit(shared_ptr<Reference> ref);

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
  /// The environment this artifact is managed by
  Env& _env;

  /// The name of this artifact used for pretty-printing
  string _name;

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;

  /// The number of versions in the sequence that have been committed to the filesystem
  size_t _committed_versions;

  shared_ptr<Version> _metadata_version;  //< The latest metadata version
  shared_ptr<Command> _metadata_creator;  //< The command that last changed this artifact's metadata
  shared_ptr<Reference> _metadata_ref;    //< The reference that was last used to change metadata
  bool _metadata_accessed = false;        //< Has this artifact's metadata been accessed?

  shared_ptr<Version> _content_version;  //< The latest content version
  shared_ptr<Command> _content_creator;  //< The command that last changed this artifact's content
  shared_ptr<Reference> _content_ref;    //< The reference that was last used to change content
  bool _content_accessed = false;        //< Has this artifact's content been accessed?
};
