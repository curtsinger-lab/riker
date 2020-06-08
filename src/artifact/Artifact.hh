#pragma once

#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "ui/options.hh"
#include "util/log.hh"

using std::list;
using std::make_shared;
using std::ostream;
using std::shared_ptr;
using std::string;

class Command;
class ContentVersion;
class Env;
class MetadataVersion;
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
   * \param committed Does the initial version of this artifact represent the filesystem state?
   * \param v         An initial version the new artifact should be seeded with
   */
  Artifact(Env& env, bool committed, shared_ptr<MetadataVersion> v);

  // Required virtual destructor
  virtual ~Artifact() = default;

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  /// Get the name of this artifact used for pretty-printing
  const string& getName() const { return _name; }

  /// Set the name of this artifact used for pretty-printing. Prefer shorter names.
  void setName(string newname) {
    if (newname.empty()) return;
    if (_name.empty() || newname.size() < _name.size()) _name = newname;
  }

  /// Get the number of versions of this artifact
  size_t getVersionCount() const { return _versions.size(); }

  /// Get the list of versions of this artifact
  const list<shared_ptr<Version>>& getVersions() const { return _versions; }

  /// Do we have a saved copy of this artifact that can be committed to the filesystem?
  virtual bool isSaved() const;

  /// Save a copy of this artifact's versions so it can be restored on a future build
  virtual void save(shared_ptr<Reference> ref);

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const;

  /// Commit any un-committed version of this artifact using the provided reference
  virtual void commit(shared_ptr<Reference> ref);

  /// Do we have a fingerprint of this artifact's versions that will allow us to check for a match?
  virtual bool hasFingerprint() const;

  /// Save a fingerprint of this artifact's versions so we can check for a match
  virtual void fingerprint(shared_ptr<Reference> ref);

  /// Check this artifact's final state against the filesystem and report any changes
  virtual void checkFinalState(shared_ptr<Reference> ref);

  /********** Metadata: All Artifact Types **********/

  /// Get the creator of the latest version of this artifact
  shared_ptr<Command> getMetadataCreator() const { return _metadata_filter.getLastWriter(); }

  /**
   * Command c accesses the metadata for this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  shared_ptr<MetadataVersion> accessMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /**
   * Command c sets the metadata of this artifact to version v using reference ref.
   * \param c   The command making the change
   * \param ref The reference used to reach this artifact
   * \param v   The version this artifact's metadata is set to, or null if the version is on disk
   * \returns the newly-assigned metadata version
   */
  shared_ptr<MetadataVersion> setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                          shared_ptr<MetadataVersion> v = nullptr);

  /********** Content: Files and Pipes **********/

  /// Get the creator of the latest version of this artifact
  virtual shared_ptr<Command> getContentCreator() const {
    WARN << "Invalid reference to contents of artifact " << this;
    return nullptr;
  }

  /**
   * Command c accesses the content of this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  virtual shared_ptr<ContentVersion> accessContents(shared_ptr<Command> c,
                                                    shared_ptr<Reference> ref) {
    WARN << "Invalid reference to contents of artifact " << this << " with reference " << ref;
    return nullptr;
  }

  /**
   * Command c sets the content of this artifact to version v using reference ref.
   * \param c   The command making the change
   * \param ref The reference used to reach this artifact
   * \param v   The version this artifact's content is set to, or null if the version is on disk
   * \returns the newly-assigned content version
   */
  virtual shared_ptr<ContentVersion> setContents(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                                 shared_ptr<ContentVersion> v = nullptr) {
    WARN << "Invalid reference to contents of artifact " << this << " with reference " << ref;
    return nullptr;
  }

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
  void appendVersion(shared_ptr<Version> v);

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

  /// The latest metadata version
  shared_ptr<MetadataVersion> _metadata_version;

  /// Is the latest metadata version committed to the filesystem?
  bool _metadata_committed;

  /// The access filter that controls metadata interactions
  AccessFilter _metadata_filter;
};
