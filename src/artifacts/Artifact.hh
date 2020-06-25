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
class LinkVersion;
class MetadataVersion;
class Reference;
class SymlinkVersion;
class UnlinkVersion;
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
   * \param v         An initial version the new artifact should be seeded with
   */
  Artifact(Env& env, shared_ptr<MetadataVersion> v) noexcept;

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

  /**
   * Check if an access to this artifact with the provided flags is allowed.
   * \param c     The command that depends on this access check
   * \param flags The flags that encode whether this is a read, write, and/or execute access
   * \returns true if the access is allowed, or false otherwise
   */
  bool checkAccess(shared_ptr<Command> c, shared_ptr<Reference> ref, AccessFlags flags) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept = 0;

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept;

  /// Can this artifact be fully committed?
  virtual bool canCommit() const noexcept;

  /// Commit any un-committed version of this artifact using the provided reference
  virtual void commit(shared_ptr<Reference> ref) noexcept;

  /// Check the final state of this artifact and save any necessary final fingerprints
  virtual void finalize(shared_ptr<Reference> ref, bool commit) noexcept;

  /// A command depends on all current versions of this artifact
  virtual void needsCurrentVersions(shared_ptr<Command> c, shared_ptr<Reference> ref) noexcept;

  /************ Metadata Operations ************/

  /// Get the current metadata version for this artifact
  shared_ptr<MetadataVersion> getMetadata(shared_ptr<Command> c,
                                          shared_ptr<Reference> ref,
                                          InputType t) noexcept;

  /// Check to see if this artifact's metadata matches a known version
  void match(shared_ptr<Command> c,
             shared_ptr<Reference> ref,
             shared_ptr<MetadataVersion> expected) noexcept;

  /// Apply a new metadata version to this artifact
  void apply(shared_ptr<Command> c,
             shared_ptr<Reference> ref,
             shared_ptr<MetadataVersion> writing) noexcept;

  /************ Content Operations ************/

  /// Get the current content version for this artifact
  virtual shared_ptr<ContentVersion> getContent(shared_ptr<Command> c,
                                                shared_ptr<Reference> ref,
                                                InputType t) noexcept {
    WARN << c << ": tried to access content of artifact " << this;
    return nullptr;
  }

  /// Check to see if this artifact's content matches a known version
  virtual void match(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<ContentVersion> expected) noexcept {
    WARN << c << ": tried to match content of artifact " << this;
  }

  /// Apply a new content version to this artifact
  virtual void apply(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<ContentVersion> writing) noexcept {
    WARN << c << ": tried to apply a content version to artifact " << this;
  }

  /************ Symlink Operations ************/

  /// Get the current symlink version of this artifact
  virtual shared_ptr<SymlinkVersion> getSymlink(shared_ptr<Command> c,
                                                shared_ptr<Reference> ref,
                                                InputType t) noexcept {
    WARN << c << ": tried to access symlink destination of artifact " << this;
    return nullptr;
  }

  /// Check to see if this artifact's symlink destination matches a known version
  virtual void match(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<SymlinkVersion> expected) noexcept {
    WARN << c << ": tried to match symlink destination of artifact " << this;
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

  /// Apply a link version to this artifact
  virtual void apply(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<LinkVersion> writing) noexcept {
    WARN << c << ": tried to apply a directory link version to artifact " << this;
  }

  /// Apply an unlink version to this artifact
  virtual void apply(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<UnlinkVersion> writing) noexcept {
    WARN << c << ": tried to apply a directory unlink version to artifact " << this;
  }

  /****** Utility Methods ******/

  /// A templated method to get the latest version of an artifact
  template <class VersionType>
  shared_ptr<VersionType> get(shared_ptr<Command> c, shared_ptr<Reference> ref, InputType t);

  /// Specialize get for metadata
  template <>
  shared_ptr<MetadataVersion> get<MetadataVersion>(shared_ptr<Command> c,
                                                   shared_ptr<Reference> ref,
                                                   InputType t) {
    return getMetadata(c, ref, t);
  }

  /// Specialize get for content
  template <>
  shared_ptr<ContentVersion> get<ContentVersion>(shared_ptr<Command> c,
                                                 shared_ptr<Reference> ref,
                                                 InputType t) {
    return getContent(c, ref, t);
  }

  /// Specialize get for symlink
  template <>
  shared_ptr<SymlinkVersion> get<SymlinkVersion>(shared_ptr<Command> c,
                                                 shared_ptr<Reference> ref,
                                                 InputType t) {
    return getSymlink(c, ref, t);
  }

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

  // Create a dummy symlink version pointer so we can still return references
  inline static shared_ptr<SymlinkVersion> _dummy_symlink_version;
};
