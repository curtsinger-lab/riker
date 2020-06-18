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
   * The provided command depends on all current versions of this artifact.
   * This method is used when a command launches with this artifact reachable through one of its
   * initial file desecriptors. The added dependency edges will ensure the artifact is in place or
   * can be committed.
   */
  virtual void needsCurrentVersions(shared_ptr<Command> c) noexcept;

  /********** Metadata: All Artifact Types **********/

  /**
   * Command c accesses the metadata for this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \param t   The type of action that led to this input
   * \returns the version the command observes
   */
  const shared_ptr<MetadataVersion>& accessMetadata(shared_ptr<Command> c,
                                                    shared_ptr<Reference> ref,
                                                    InputType t) noexcept;

  /**
   * Command c sets the metadata of this artifact to version v using reference ref.
   * \param c   The command making the change
   * \param ref The reference used to reach this artifact
   * \param v   The version this artifact's metadata is set to, or null if the version is on disk
   * \returns the newly-assigned metadata version
   */
  const shared_ptr<MetadataVersion>& setMetadata(shared_ptr<Command> c,
                                                 shared_ptr<Reference> ref,
                                                 shared_ptr<MetadataVersion> v = nullptr) noexcept;

  /**
   * Check if an access to this artifact with the provided flags is allowed.
   * \param c     The command that depends on this access check
   * \param flags The flags that encode whether this is a read, write, and/or execute access
   * \returns true if the access is allowed, or false otherwise
   */
  bool checkAccess(shared_ptr<Command> c, AccessFlags flags) noexcept;

  /********** Content: Files and Pipes **********/

  /**
   * Command c accesses the content of this artifact using reference ref.
   * \param c   The command making the access
   * \param ref The referenced used to reach this artifact
   * \returns the version the command observes, or nullptr if the command has already observed the
   *          latest version using this reference (no check is necessary).
   */
  virtual const shared_ptr<ContentVersion>& accessContents(shared_ptr<Command> c,
                                                           shared_ptr<Reference> ref) noexcept {
    WARN << "Invalid reference to contents of artifact " << this << " with reference " << ref;
    return _dummy_content_version;
  }

  /**
   * Command c sets the content of this artifact to version v using reference ref.
   * \param c   The command making the change
   * \param ref The reference used to reach this artifact
   * \param v   The version this artifact's content is set to, or null if the version is on disk
   * \returns the newly-assigned content version
   */
  virtual const shared_ptr<ContentVersion>& setContents(
      shared_ptr<Command> c,
      shared_ptr<Reference> ref,
      shared_ptr<ContentVersion> v = nullptr) noexcept {
    WARN << "Invalid reference to contents of artifact " << this << " with reference " << ref;
    return _dummy_content_version;
  }

  /************ Symlink Operations ************/

  virtual const shared_ptr<SymlinkVersion>& readlink(shared_ptr<Command> c, InputType t) noexcept {
    WARN << "Invalid readlink call on non-symlink artifact";
    return _dummy_symlink_version;
  }

  /************ Directory Operations ************/

  /**
   * Attempt to access a directory entry in the current artifact.
   * \param self  The path that was used to reach this artifact
   * \param entry The name of the entry being requested
   * \returns a resolution result, holding either an artifact or error code
   */
  virtual Resolution getEntry(shared_ptr<Command> c, fs::path dirpath, fs::path entry) noexcept {
    // This is not a directory, so the access always fails
    return ENOTDIR;
  }

  virtual void addEntry(shared_ptr<Command> c,
                        fs::path dirpath,
                        fs::path entry,
                        shared_ptr<Reference> target) noexcept {}

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

  /// Create a dummy content version pointer so we can still return references
  inline static shared_ptr<ContentVersion> _dummy_content_version;

  // Create a dummy symlink version pointer so we can still return references
  inline static shared_ptr<SymlinkVersion> _dummy_symlink_version;
};
