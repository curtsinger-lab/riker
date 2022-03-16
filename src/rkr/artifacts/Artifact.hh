#pragma once

#include <cstddef>
#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>

#include "data/AccessFlags.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "runtime/VersionState.hh"
#include "util/log.hh"
#include "versions/MetadataVersion.hh"

namespace fs = std::filesystem;

class Build;
class ContentVersion;
class DirArtifact;
class DirEntry;
class DirVersion;
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
   * Create a new artifact with no existing metadata. This should be followed with updateContent and
   * updateMetadata calls to set initial state for the artifact.
   */
  Artifact() noexcept;

  /**
   * Create a new artifact with an existing metadata version. This is only appropriate for artifacts
   * that exist on the filesystem.
   * \param v The new artifact's committed metadata version
   */
  Artifact(MetadataVersion v) noexcept;

  // Required virtual destructor
  virtual ~Artifact() noexcept = default;

  // Disallow Copy
  Artifact(const Artifact&) = delete;
  Artifact& operator=(const Artifact&) = delete;

  /// Try to cast this artifact to some subtype
  template <class T>
  std::shared_ptr<T> as() noexcept {
    return std::dynamic_pointer_cast<T>(shared_from_this());
  }

  /// Try to cast this artifact to some subtype (const version)
  template <class T>
  std::shared_ptr<const T> as() const noexcept {
    return std::dynamic_pointer_cast<const T>(shared_from_this());
  }

  /// Get a pretty-printed name for this artifact
  std::string getName() const noexcept;

  /// Set the name of this artifact used for pretty-printing
  void setName(std::string newname) noexcept { _name = newname; }

  /// Get a list of all the versions associated with this artifact
  const std::list<std::shared_ptr<Version>>& getVersions() const noexcept { return _versions; }

  /// Get a file descriptor for this artifact
  virtual int getFD(AccessFlags flags) noexcept;

  /**
   * Check if an access to this artifact with the provided flags is allowed.
   * \param c     The command that depends on this access check
   * \param flags The flags that encode whether this is a read, write, and/or execute access
   * \returns true if the access is allowed, or false otherwise
   */
  bool checkAccess(const std::shared_ptr<Command>& c, AccessFlags flags) noexcept;

  /// This artifact is the root directory
  void setRootDir() noexcept { _root_dir = true; }

  /// Revert this artifact to its committed state
  virtual void rollback() noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual std::string getTypeName() const noexcept = 0;

  /// Commit this artifact's metadata to the filesystem
  virtual void commitMetadata() noexcept;

  /// Commit the content of this artifact to the filesystem
  void commitContent() noexcept;

  /// Commit the content of this artifact to a specific path
  virtual void commitContentTo(fs::path path) noexcept = 0;

  /// Does this artifact have any uncommitted content?
  virtual bool hasUncommittedContent() noexcept = 0;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept = 0;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept;

  /// Fingerprint and cache the committed state of this artifact
  virtual void cacheAll(fs::path path) const noexcept {};

  /************ Path Manipulation ************/

  /// Model a link to this artifact, but do not commit it to the filesystem
  void addLink(std::shared_ptr<DirEntry> entry) noexcept;

  /// Model an unlink of this artifact, but do not commit it to the filesystem
  void removeLink(std::shared_ptr<DirEntry> entry) noexcept;

  /// Add an already-committed link to this artifact
  void addCommittedLink(std::shared_ptr<DirEntry> entry) noexcept;

  /// Remove a committed link from this artifact
  void removeCommittedLink(std::shared_ptr<DirEntry> addEntry) noexcept;

  /// Commit a link to this artifact at the given path
  virtual void commitLink(std::shared_ptr<DirEntry> entry) noexcept = 0;

  /// Commit an unlink of this artifact at the given path
  virtual void commitUnlink(std::shared_ptr<DirEntry> entry) noexcept = 0;

  /// Get a committed path to this artifact
  std::optional<fs::path> getCommittedPath() const noexcept;

  /// Get a path to this artifact, either committed or uncommitted
  std::optional<fs::path> getPath() const noexcept;

  /// Get a parent directory for this artifact. The result may or may not be on the filesystem
  std::optional<std::shared_ptr<DirArtifact>> getParentDir() noexcept;

  /// Get a committed path to this artifact, and commit one if necessary
  std::optional<fs::path> commitPath() noexcept;

  /************ Metadata Operations ************/

  /// Get the current metadata version for this artifact
  std::shared_ptr<MetadataVersion> getMetadata(const std::shared_ptr<Command>& c) noexcept;

  /// Get the current metadata without recording any dependencies
  std::shared_ptr<MetadataVersion> peekMetadata() noexcept;

  /// Check to see if this artifact's metadata matches a known version
  void matchMetadata(const std::shared_ptr<Command>& c,
                     Scenario scenario,
                     std::shared_ptr<MetadataVersion> expected) noexcept;

  /// Apply a new metadata version to this artifact
  void updateMetadata(const std::shared_ptr<Command>& c,
                      std::shared_ptr<MetadataVersion> writing) noexcept;

  /************ Traced Operations ************/

  /// A traced command is about to stat this artifact
  virtual void beforeStat(Build& build, const std::shared_ptr<Command>& c, Ref::ID ref) noexcept {}

  /// A traced command is about to close a reference to this artifact
  virtual void beforeClose(Build& build, const std::shared_ptr<Command>& c, Ref::ID ref) noexcept {}

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept = 0;

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build, const std::shared_ptr<Command>& c, Ref::ID ref) noexcept = 0;

  /// A traced command is about to (possibly) write to this artifact
  virtual void beforeWrite(Build& build, const std::shared_ptr<Command>& c, Ref::ID ref) noexcept {
    FAIL << c << " attempted to write " << this;
  }

  /// A traced command just wrote to this artifact
  virtual void afterWrite(Build& build, const std::shared_ptr<Command>& c, Ref::ID ref) noexcept {
    FAIL << c << " attempted to write " << this;
  }

  /// A traced command is about to (possibly) truncate this artifact to length zero
  virtual void beforeTruncate(Build& build,
                              const std::shared_ptr<Command>& c,
                              Ref::ID ref) noexcept {
    FAIL << c << " attempted to truncate " << this;
  }

  /// A trace command just truncated this artifact to length zero
  virtual void afterTruncate(Build& build,
                             const std::shared_ptr<Command>& c,
                             Ref::ID ref) noexcept {
    FAIL << c << " attempted to truncate " << this;
  }

  /************ Content Operations ************/

  /// Get this artifact's current content
  virtual std::shared_ptr<ContentVersion> getContent(
      const std::shared_ptr<Command>& c) noexcept = 0;

  /// Get this artifact's current content without creating any dependencies
  std::shared_ptr<ContentVersion> peekContent() noexcept { return getContent(nullptr); }

  /// Check to see if this artifact's content matches a known version
  virtual void matchContent(const std::shared_ptr<Command>& c,
                            Scenario scenario,
                            std::shared_ptr<ContentVersion> expected) noexcept = 0;

  /// Update this artifact's content with a file version
  virtual void updateContent(const std::shared_ptr<Command>& c,
                             std::shared_ptr<ContentVersion> writing) noexcept {
    WARN << c << ": tried to apply a content version to artifact " << this;
  }

  /************ Directory Operations ************/

  /// Add a directory entry to this artifact
  virtual void addEntry(const std::shared_ptr<Command>& c,
                        std::string name,
                        std::shared_ptr<Artifact> target) noexcept {
    WARN << c << ": tried to add an entry to non-directory artifact " << this;
  }

  /// Remove a directory entry from this artifact
  virtual void removeEntry(const std::shared_ptr<Command>& c,
                           std::string name,
                           std::shared_ptr<Artifact> target) noexcept {
    WARN << c << ": tried to remove an entry from non-directory artifact " << this;
  }

  /**
   * Resolve a path relative to this artifact
   * \param c     The command this resolution is performed on behalf of
   * \param path  The path being resolved
   * \param flags The access mode requested
   * \param symlink_limit Don't follow symlinks deeper than this number of levels
   * \returns a resolution result, which is either an artifact or an error code
   */
  Ref resolve(const std::shared_ptr<Command>& c,
              fs::path path,
              AccessFlags flags,
              size_t symlink_limit = 40) noexcept {
    return resolve(c, nullptr, path.begin(), path.end(), flags, symlink_limit);
  }

  /**
   * Resolve a path relative to this artifact.
   * \param c         The command this resolution is performed on behalf of
   * \param prev      The previously-visited artifact along this path. This won't always be the
   *                    parent directory, since paths can include ".." entries
   * \param current   An iterator to the next part of the path to be resolved
   * \param end       An iterator to the end of the path
   * \param flags     The access mode requested for the final resolved file
   * \param symlink_limit Don't follow symlinks deeper than this number of levels
   * \returns a resolution result, which is either an artifact or an error code
   */
  virtual Ref resolve(const std::shared_ptr<Command>& c,
                      std::shared_ptr<Artifact> prev,
                      fs::path::iterator current,
                      fs::path::iterator end,
                      AccessFlags flags,
                      size_t symlink_limit) noexcept;

  /****** Utility Methods ******/

  /// Print this artifact
  friend std::ostream& operator<<(std::ostream& o, const Artifact& a) noexcept {
    o << "[" << a.getTypeName();
    auto name = a.getName();
    if (!name.empty()) o << " " << name;
    o << "]";
    return o;
  }

  /// Print a pointer to an artifact
  friend std::ostream& operator<<(std::ostream& o, const Artifact* a) noexcept {
    if (a == nullptr) return o << "<null Artifact>";
    return o << *a;
  }

 protected:
  /// Commit this artifact's metadata to a specific path
  virtual void commitMetadataTo(fs::path path) noexcept;

  /// Set a temporary path for this artifact and return it
  fs::path assignTemporaryPath() noexcept;

  /// Take the temporary path from this artifact and return it
  std::optional<fs::path> takeTemporaryPath() noexcept;

  /// Remember a version associated with this artifact
  void appendVersion(std::shared_ptr<Version> v) noexcept;

 private:
  /// If true, this artifact is the root directory and has path "/"
  bool _root_dir = false;

  /// A fixed string name assigned to this artifact
  std::string _name;

  /// All of the versions of this artifact
  std::list<std::shared_ptr<Version>> _versions;

  /// A path to a temporary location where this artifact is linked
  std::optional<fs::path> _temp_path;

 protected:
  /// The committed and uncommitted metadata for this artifact
  VersionState<MetadataVersion> _metadata;

  using LinkSet = std::set<std::weak_ptr<DirEntry>, std::owner_less<std::weak_ptr<DirEntry>>>;

  /// The set of links to this artifact currently available on the filesystem
  LinkSet _committed_links;

  /// The set of links to this artifact in the filesystem model. This will include committed links
  /// unless they have been unlinked in the model but that unlink has not been committed.
  LinkSet _modeled_links;
};
