#pragma once

#include <filesystem>
#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <string>

#include "build/AccessTypes.hh"
#include "build/Resolution.hh"
#include "core/AccessFlags.hh"
#include "ui/options.hh"
#include "util/log.hh"

using std::list;
using std::make_shared;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

namespace fs = std::filesystem;

class Access;
class Command;
class ContentVersion;
class Env;
class MetadataVersion;
class Reference;
class SymlinkVersion;
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
  Artifact(Env& env, bool committed, shared_ptr<MetadataVersion> v) noexcept;

  // Required virtual destructor
  virtual ~Artifact() noexcept = default;

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  /// Try to cast this artifact to some subtype
  template <class T>
  shared_ptr<T> as() noexcept {
    return std::dynamic_pointer_cast<T>(shared_from_this());
  }

  /// Get the name of this artifact used for pretty-printing
  const string& getName() const noexcept { return _name; }

  /// Set the name of this artifact used for pretty-printing. Prefer shorter names.
  void setName(string newname) noexcept {
    if (newname.empty()) return;
    if (_name.empty() || newname.size() < _name.size()) _name = newname;
  }

  /// Get the number of versions of this artifact
  size_t getVersionCount() const noexcept { return _versions.size(); }

  /// Get the list of versions of this artifact
  const list<shared_ptr<Version>>& getVersions() const noexcept { return _versions; }

  /// Do we have a saved copy of this artifact that can be committed to the filesystem?
  virtual bool isSaved() const noexcept;

  /// Save a copy of this artifact's versions so it can be restored on a future build
  virtual void save(shared_ptr<Reference> ref) noexcept;

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept;

  /// Commit any un-committed version of this artifact using the provided reference
  virtual void commit(shared_ptr<Reference> ref) noexcept;

  /// Do we have a fingerprint of this artifact's versions that will allow us to check for a match?
  virtual bool hasFingerprint() const noexcept;

  /// Save a fingerprint of this artifact's versions so we can check for a match
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept;

  /// Check the final state of this artifact and save any necessary final fingerprints
  virtual void finalize(shared_ptr<Reference> ref) noexcept;

  /**
   * Check if an access to this artifact with the provided flags is allowed.
   * \param c     The command that depends on this access check
   * \param flags The flags that encode whether this is a read, write, and/or execute access
   * \returns true if the access is allowed, or false otherwise
   */
  bool checkAccess(shared_ptr<Command> c, AccessFlags flags) noexcept;

  /**
   * The provided command depends on all current versions of this artifact.
   * This method is used when a command launches with this artifact reachable through one of its
   * initial file desecriptors. The added dependency edges will ensure the artifact is in place or
   * can be committed.
   */
  virtual void needsCurrentVersions(shared_ptr<Command> c) noexcept;

  /********** Input Methods **********/

  // These methods are called any time a command depends on the current state of this artifact.
  // Method parameters are:
  //   c        - the command that makes the access
  //   ref      - A reference used to reach the artifact
  //   expected - A version the reader expects to find, or nullptr if no previous read has been made
  //   t        - The category for this input
  //
  // Each method returns the version the command observed. This can be saved and used on later runs
  // to check whether the artifact is in the same state that was observed by this invocation.

  /// Command c accesses this artifact's metadata
  shared_ptr<MetadataVersion> read(shared_ptr<Command> c,
                                   shared_ptr<Reference> ref,
                                   shared_ptr<MetadataVersion> expected,
                                   InputType t) noexcept;

  /// Command c accesses this artifact's contents
  virtual shared_ptr<ContentVersion> read(shared_ptr<Command> c,
                                          shared_ptr<Reference> ref,
                                          shared_ptr<ContentVersion> expected,
                                          InputType t) noexcept {
    WARN << c << ": Invalid ContentVersion read from " << this << " with reference " << ref;
    return nullptr;
  }

  /// Command c accesses this artifact's symlink destination
  virtual shared_ptr<SymlinkVersion> read(shared_ptr<Command> c,
                                          shared_ptr<Reference> ref,
                                          shared_ptr<SymlinkVersion> expected,
                                          InputType t) noexcept {
    WARN << c << ": Invalid SymlinkVersion read from " << this << " with reference " << ref;
    return nullptr;
  }

  /********** Output Methods **********/

  // These methods are called any time a command changes the current state of this artifact.
  // Method parameters are:
  //   c       - the command that makes the access
  //   ref     - A reference used to reach the artifact
  //   writing - The version the writer is applying to this artifact
  //
  // Each method returns the version the command wrote. This can be saved and used on later runs to
  // emulate an equivalent write to this artifact.

  /// Command c sets this artifact's metadata
  shared_ptr<MetadataVersion> write(shared_ptr<Command> c,
                                    shared_ptr<Reference> ref,
                                    shared_ptr<MetadataVersion> writing) noexcept;

  /// Command c sets this artifact's contents
  virtual shared_ptr<ContentVersion> write(shared_ptr<Command> c,
                                           shared_ptr<Reference> ref,
                                           shared_ptr<ContentVersion> writing) noexcept {
    WARN << c << ": Invalid ContentVersion write to " << this << " with reference " << ref;
    return nullptr;
  }

  /************ Directory Operations ************/

  /**
   * Attempt to access a directory entry in the current artifact.
   * \param c     The command making the access
   * \param ref   A reference that was used to reach this directory
   * \param entry The name of the entry being requested
   * \returns a resolution result, holding either an artifact or error code
   */
  virtual Resolution getEntry(shared_ptr<Command> c,
                              shared_ptr<Reference> ref,
                              string entry) noexcept {
    // This is not a directory, so the access always fails
    return ENOTDIR;
  }

  /**
   * Add an entry to this directory
   * \param c      The command making the access
   * \param ref    A reference that was used to reach this directory
   * \param entry  The name of the directory entry
   * \param target A reference to the artifact that is being linked into the directory
   */
  virtual void addEntry(shared_ptr<Command> c,
                        shared_ptr<Reference> ref,
                        string entry,
                        shared_ptr<Reference> target) noexcept {}

  /**
   * Remove an entry from this directory
   * \param c      The command making the access
   * \param ref    A reference that was used to reach this directory
   * \param entry  The name of the directory entry
   */
  virtual void removeEntry(shared_ptr<Command> c,
                           shared_ptr<Reference> ref,
                           string entry) noexcept {}

  /****** Utility Methods ******/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept = 0;

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) noexcept {
    o << "[" << a.getTypeName();
    auto name = a.getName();
    if (!name.empty()) o << " " << name;
    o << "]";
    return o;
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) noexcept { return o << *a; }

 protected:
  /// Add a version to the sequence of versions for this artifact
  void appendVersion(shared_ptr<Version> v) noexcept;

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

  // Create a dummy symlink version pointer so we can still return references
  inline static shared_ptr<SymlinkVersion> _dummy_symlink_version;
};
