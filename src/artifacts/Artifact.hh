#pragma once

#include <filesystem>
#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <string>
#include <tuple>

#include "build/AccessTypes.hh"
#include "build/Resolution.hh"
#include "core/AccessFlags.hh"
#include "ui/options.hh"
#include "util/log.hh"

using std::list;
using std::make_shared;
using std::optional;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::tuple;

namespace fs = std::filesystem;

class Access;
class Command;
class DirArtifact;
class Env;
class FileVersion;
class AddEntry;
class MetadataVersion;
class Ref;
class SymlinkVersion;
class RemoveEntry;
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

  /// Get a pretty-printed name for this artifact
  string getName() const noexcept;

  /// Set the name of this artifact used for pretty-printing
  void setName(string newname) noexcept { _name = newname; }

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
  bool checkAccess(shared_ptr<Command> c, AccessFlags flags) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept = 0;

  /// Can a specific version of this artifact be committed?
  virtual bool canCommit(shared_ptr<Version> v) const noexcept;

  /// Commit a specific version (and any co-dependent versions) to the filesystem
  virtual void commit(shared_ptr<Version> v) noexcept;

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll() noexcept;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState() noexcept;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState() noexcept;

  /************ Path Manipulation ************/

  /// Notify this artifact that it is linked to a parent directory with a given entry name.
  /// If committed is true, the link is already in place on the filesystem.
  virtual void linkAt(shared_ptr<DirArtifact> dir, string entry, bool committed = false) noexcept;

  /// Update the filesystem so this artifact is linked in the given directory
  virtual void commitLinkAt(shared_ptr<DirArtifact> dir, string entry) noexcept;

  /// Notify this artifact that it is unlinked from ap arent directory at a given entry name.
  /// If committed is true, the link has already been removed on the filesystem.
  virtual void unlinkAt(shared_ptr<DirArtifact> dir, string entry, bool committed = false) noexcept;

  /// Update the filesystem so this artifact is no longer linked in the given directory
  virtual void commitUnlinkAt(shared_ptr<DirArtifact> dir, string entry) noexcept;

  /// Get a reasonable path to this artifact. The path may not by in place on the filesystem, but
  /// the path will reflect a location of this artifact at some point during the build.
  optional<fs::path> getPath() const noexcept;

  /// Get a committed path to this artifact. The path may be a temporary location that does not
  /// appear during the build, but this artifact is guaranteed to be at that path.
  optional<fs::path> getCommittedPath() const noexcept;

  /// Get a parent directory for this artifact. The result may or may not be on the filesystem
  optional<DirArtifact*> getParentDir() const noexcept;

  /// Get the set of uncommitted links to this artifact
  const set<tuple<DirArtifact*, string>>& getUncommittedLinks() const noexcept {
    return _uncommitted_links;
  }

  /// Get the set of committed links to this artifact
  const set<tuple<DirArtifact*, string>>& getCommittedLinks() const noexcept {
    return _committed_links;
  }

  /************ Metadata Operations ************/

  /// Get the current metadata version for this artifact
  shared_ptr<MetadataVersion> getMetadata(shared_ptr<Command> c, InputType t) noexcept;

  /// Check to see if this artifact's metadata matches a known version
  void match(shared_ptr<Command> c, shared_ptr<MetadataVersion> expected) noexcept;

  /// Apply a new metadata version to this artifact
  void apply(shared_ptr<Command> c, shared_ptr<MetadataVersion> writing) noexcept;

  /************ Content Operations ************/

  /// Get the current content version for this artifact
  virtual shared_ptr<FileVersion> getContent(shared_ptr<Command> c, InputType t) noexcept {
    WARN << c << ": tried to access content of artifact " << this;
    return nullptr;
  }

  /// Check to see if this artifact's content matches a known version
  virtual void match(shared_ptr<Command> c, shared_ptr<FileVersion> expected) noexcept {
    WARN << c << ": tried to match content of artifact " << this;
  }

  /// Apply a new content version to this artifact
  virtual void apply(shared_ptr<Command> c, shared_ptr<FileVersion> writing) noexcept {
    WARN << c << ": tried to apply a content version to artifact " << this;
  }

  /************ Symlink Operations ************/

  /// Get the current symlink version of this artifact
  virtual shared_ptr<SymlinkVersion> getSymlink(shared_ptr<Command> c, InputType t) noexcept {
    WARN << c << ": tried to access symlink destination of artifact " << this;
    return nullptr;
  }

  /// Check to see if this artifact's symlink destination matches a known version
  virtual void match(shared_ptr<Command> c, shared_ptr<SymlinkVersion> expected) noexcept {
    WARN << c << ": tried to match symlink destination of artifact " << this;
  }

  /************ Directory Operations ************/

  /**
   * Resolve a path relative to this artifact.
   * \param c         The command this resolution is performed on behalf of
   * \param prev      The previously-visited artifact along this path. This won't always be the
   *                    parent directory, since paths can include ".." entries
   * \param current   An iterator to the next part of the path to be resolved
   * \param end       An iterator to the end of the path
   * \param ref       The reference that is being resolved
   * \param committed If true, any accesses or actions taken during resolution should be committed.
   * \returns a resolution result, which is either an artifact or an error code
   */
  virtual Resolution resolve(shared_ptr<Command> c,
                             shared_ptr<Artifact> prev,
                             fs::path::iterator current,
                             fs::path::iterator end,
                             shared_ptr<Access> ref,
                             bool committed) noexcept;

  /// Apply a link version to this artifact
  virtual void apply(shared_ptr<Command> c, shared_ptr<AddEntry> writing) noexcept {
    WARN << c << ": tried to apply a directory link version to artifact " << this;
  }

  /// Apply an unlink version to this artifact
  virtual void apply(shared_ptr<Command> c, shared_ptr<RemoveEntry> writing) noexcept {
    WARN << c << ": tried to apply a directory unlink version to artifact " << this;
  }

  /****** Utility Methods ******/

  /// A templated method to get the latest version of an artifact
  template <class VersionType>
  shared_ptr<VersionType> get(shared_ptr<Command> c, InputType t);

  /// Specialize get for metadata
  template <>
  shared_ptr<MetadataVersion> get<MetadataVersion>(shared_ptr<Command> c, InputType t) {
    return getMetadata(c, t);
  }

  /// Specialize get for content
  template <>
  shared_ptr<FileVersion> get<FileVersion>(shared_ptr<Command> c, InputType t) {
    return getContent(c, t);
  }

  /// Specialize get for symlink
  template <>
  shared_ptr<SymlinkVersion> get<SymlinkVersion>(shared_ptr<Command> c, InputType t) {
    return getSymlink(c, t);
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

  /// The latest metadata version
  shared_ptr<MetadataVersion> _metadata_version;

  /// A set of links to this artifact that have not been committed
  set<tuple<DirArtifact*, string>> _uncommitted_links;

  /// A set of links to this artifact that are in place on the filesystem
  set<tuple<DirArtifact*, string>> _committed_links;

 private:
  /// A fixed string name assigned to this artifact
  string _name;

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;
};
