#pragma once

#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "ui/options.hh"

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
  Artifact(Env& env, bool committed, shared_ptr<Version> v = make_shared<Version>());

  // Required virtual destructor
  virtual ~Artifact() = default;

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  /// Get the name of this artifact used for pretty-printing
  const string& getName() const { return _name; }

  /// Set the naem of this artifact used for pretty-printing
  void setName(string name) { _name = name; }

  /// Get the number of versions of this artifact
  size_t getVersionCount() const { return _versions.size(); }

  /// Get the list of versions of this artifact
  const list<shared_ptr<Version>>& getVersions() const { return _versions; }

  /// Have all modifications to this artifact been committed to the filesystem?
  bool isCommitted() const { return _committed_versions == _versions.size(); }

  /// Do we have sufficient saved data to commit this artifact to the filesystem?
  bool canCommit() const;

  /// Commit any un-committed version of this artifact using the provided reference
  void commit(shared_ptr<Reference> ref);

  /// Check this artifact's final state against the filesystem and report any changes
  virtual void checkFinalState(shared_ptr<Reference> ref);

  /********** Metadata: All Artifact Types **********/

  /// Get the creator of the latest version of this artifact
  shared_ptr<Command> getMetadataCreator() const { return _metadata_filter.getLastWriter(); }

  /**
   * Save the metadata for the latest version of this artifact
   * \param ref The reference to this artifact that should be used to access metadata
   */
  void saveMetadata(shared_ptr<Reference> ref);

  /**
   * Command c accesses the metadata for this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  shared_ptr<Version> accessMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /**
   * Command c sets the metadata of this artifact to version v using reference ref.
   * \param c   The command making the change
   * \param ref The reference used to reach this artifact
   * \param v   The version this artifact's metadata is set to, or null if the version is on disk
   * \returns the newly-assigned metadata version
   */
  shared_ptr<Version> setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                  shared_ptr<Version> v = nullptr);

  /********** Content: Files and Pipes **********/

  /// Get the creator of the latest version of this artifact
  virtual shared_ptr<Command> getContentCreator() const = 0;

  /**
   * Save a fingerprint of the contents of the latest version of this artifact
   * \param ref The reference to this artifact that should be used to access contents
   */
  virtual void saveFingerprint(shared_ptr<Reference> ref) = 0;

  /**
   * Command c accesses the content of this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  virtual shared_ptr<Version> accessContents(shared_ptr<Command> c, shared_ptr<Reference> ref) = 0;

  /**
   * Command c sets the content of this artifact to version v using reference ref.
   * \param c   The command making the change
   * \param ref The reference used to reach this artifact
   * \param v   The version this artifact's content is set to, or null if the version is on disk
   * \returns the newly-assigned content version
   */
  virtual shared_ptr<Version> setContents(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                          shared_ptr<Version> v = nullptr) = 0;

  /****** Utility Methods ******/

  /// Get the name of this artifact type
  virtual string getTypeName() const = 0;

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) {
    o << "[" << a.getTypeName();
    auto name = a.getName();
    if (!name.empty()) o << " " << name;
    o << "]@v" << a._versions.size() - 1;
    return o;
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) { return o << *a; }

 protected:
  /// Add a version to the sequence of versions for this artifact
  void appendVersion(shared_ptr<Version> v, bool committed);

  /**
   * This class captures the state and logic required to decide when reads/writes must be recorded
   * and when they can safely be skipped.
   */
  class AccessFilter {
   public:
    /**
     * Update tracking data to record that the given command has made a read access
     * \param reader The command that issued the read
     */
    void readBy(shared_ptr<Command> reader);

    /**
     * Check if a read access must be logged, or if it can safely be elided from the trace.
     * \param reader The command that is issuing the read
     * \param ref    The reference used for the read
     * \returns true if the read must be logged, or false otherwise
     */
    bool readRequired(shared_ptr<Command> reader, shared_ptr<Reference> ref);

    /**
     * Update tracking data to record that the given command has made a write access
     * \param writer The command that issued the write
     * \param ref    The reference used for the write
     */
    void writtenBy(shared_ptr<Command> writer, shared_ptr<Reference> ref);

    /**
     * Check if a write access must be logged, or if it can safely be elided from the trace
     * \param writer The command that issued the write
     * \param ref    The reference used for the write
     * \return true if the write must be logged with a new version, or false otherwise
     */
    bool writeRequired(shared_ptr<Command> writer, shared_ptr<Reference> ref);

    /// Get the last writer
    shared_ptr<Command> getLastWriter() const { return _last_writer; }

   private:
    shared_ptr<Command> _last_writer;
    shared_ptr<Reference> _write_ref;
    bool _accessed = false;
  };

 protected:
  /// The environment this artifact is managed by
  Env& _env;

 private:
  /// The name of this artifact used for pretty-printing
  string _name;

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;

  /// The number of versions in the sequence that have been committed to the filesystem
  size_t _committed_versions = 0;

  /// The latest metadata version
  shared_ptr<Version> _metadata_version;

  /// The access filter that controls metadata interactions
  AccessFilter _metadata_filter;
};
