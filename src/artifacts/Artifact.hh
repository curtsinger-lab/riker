#pragma once

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>

#include "data/AccessFlags.hh"
#include "runtime/CommandRun.hh"
#include "runtime/Ref.hh"
#include "ui/options.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"

using std::list;
using std::make_shared;
using std::map;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::weak_ptr;

namespace fs = std::filesystem;

class AddEntry;
class Build;
class Command;
class DirArtifact;
class DirListVersion;
class DirVersion;
class Env;
class FileVersion;
class MetadataVersion;
class PathRef;
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
  Artifact(shared_ptr<Env> env, shared_ptr<MetadataVersion> v) noexcept;

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
  size_t getVersionCount() const noexcept {
    return _metadata_versions.size() + _content_versions.size();
  }

  /// Get a list of all metadata versions that have been set for this artifact
  const list<shared_ptr<MetadataVersion>>& getMetadataVersions() const noexcept {
    return _metadata_versions;
  }

  const list<shared_ptr<ContentVersion>>& getContentVersions() const noexcept {
    return _content_versions;
  }

  /// Get the environment this artifact is part of
  shared_ptr<Env> getEnv() const noexcept { return _env.lock(); }

  /// Get a file descriptor for this artifact
  virtual int getFD(AccessFlags flags) noexcept;

  /**
   * Check if an access to this artifact with the provided flags is allowed.
   * \param c     The command that depends on this access check
   * \param flags The flags that encode whether this is a read, write, and/or execute access
   * \returns true if the access is allowed, or false otherwise
   */
  bool checkAccess(const shared_ptr<Command>& c, AccessFlags flags) noexcept;

  /// Commit links to ensure there is at least one committed path to this artifact
  optional<fs::path> commitPath() noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept = 0;

  /// Can this artifact's content version be committed?
  virtual bool canCommit(shared_ptr<ContentVersion> v) const noexcept = 0;

  /// Commit this artifact's metadata version
  virtual void commitMetadata() noexcept = 0;

  /// Commit a specific version (and any co-dependent versions) to the filesystem
  virtual void commit(shared_ptr<ContentVersion> v) noexcept = 0;

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept = 0;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll() noexcept = 0;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept = 0;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept;

  /// Mark all versions and paths to this artifact as committed
  virtual void setCommitted() noexcept;

  /************ Path Manipulation ************/

  /// A link is a tuple of a directory and an entry name in that directory
  using Link = tuple<shared_ptr<DirArtifact>, fs::path>;

  /// Inform this artifact that it is linked or unlinked
  void addLinkUpdate(shared_ptr<DirArtifact> dir,
                     fs::path entry,
                     shared_ptr<DirVersion> v) noexcept;

  /// Get all paths to this artifact. Returns two maps, each of which map a link (directory and
  /// entry) to the version that creates that link. The first map holds committed links, while the
  /// second map holds uncommitted links.
  tuple<map<Link, shared_ptr<DirVersion>>, map<Link, shared_ptr<DirVersion>>> getLinks()
      const noexcept;

  /// Get a path to this artifact that may or may not be committed to the filesystem
  optional<fs::path> getPath(bool allow_uncommitted = true) const noexcept;

  /// Get a parent directory for this artifact. The result may or may not be on the filesystem
  optional<shared_ptr<DirArtifact>> getParentDir() noexcept;

  /// Generate and save a temporary path for this artifact. Returns the new path.
  /// The caller must make sure this artifact is linked at the new temporary path.
  fs::path assignTemporaryPath() noexcept;

  /// Clear the temporary path for this artifact. Returns the old temporary path if it had one.
  optional<fs::path> takeTemporaryPath() noexcept;

  /************ Metadata Operations ************/

  /// Get the current metadata version for this artifact
  shared_ptr<MetadataVersion> getMetadata(const shared_ptr<Command>& c, InputType t) noexcept;

  /// Get the current metadata without recording any dependencies
  shared_ptr<MetadataVersion> peekMetadata() noexcept;

  /// Check to see if this artifact's metadata matches a known version
  void matchMetadata(const shared_ptr<Command>& c,
                     Scenario scenario,
                     shared_ptr<MetadataVersion> expected) noexcept;

  /// Apply a new metadata version to this artifact
  shared_ptr<MetadataVersion> updateMetadata(
      const shared_ptr<Command>& c,
      shared_ptr<MetadataVersion> writing = nullptr) noexcept;

  /************ Traced Operations ************/

  /// A traced command is about to close a reference to this artifact
  virtual void beforeClose(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {}

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept = 0;

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept = 0;

  /// A traced command is about to (possibly) write to this artifact
  virtual void beforeWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
    FAIL << c << " attempted to write " << this;
  }

  /// A traced command just wrote to this artifact
  virtual void afterWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
    FAIL << c << " attempted to write " << this;
  }

  /// A traced command is about to (possibly) truncate this artifact to length zero
  virtual void beforeTruncate(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
    FAIL << c << " attempted to truncate " << this;
  }

  /// A trace command just truncated this artifact to length zero
  virtual void afterTruncate(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
    FAIL << c << " attempted to truncate " << this;
  }

  /************ Content Operations ************/

  /// Get this artifact's current content without creating any dependencies
  virtual shared_ptr<ContentVersion> peekContent() noexcept = 0;

  /// Check to see if this artifact's content matches a known version
  virtual void matchContent(const shared_ptr<Command>& c,
                            Scenario scenario,
                            shared_ptr<ContentVersion> expected) noexcept = 0;

  /// Update this artifact's content with a file version
  virtual void updateContent(const shared_ptr<Command>& c,
                             shared_ptr<ContentVersion> writing) noexcept {
    WARN << c << ": tried to apply a content version to artifact " << this;
  }

  /************ Directory Operations ************/

  /// Add a directory entry to this artifact
  virtual shared_ptr<DirVersion> addEntry(const shared_ptr<Command>& c,
                                          fs::path entry,
                                          shared_ptr<Artifact> target) noexcept {
    WARN << c << ": tried to add an entry to non-directory artifact " << this;
    return nullptr;
  }

  /// Remove a directory entry from this artifact
  virtual shared_ptr<DirVersion> removeEntry(const shared_ptr<Command>& c,
                                             fs::path entry,
                                             shared_ptr<Artifact> target) noexcept {
    WARN << c << ": tried to remove an entry from non-directory artifact " << this;
    return nullptr;
  }

  /**
   * Resolve a path relative to this artifact
   * \param c     The command this resolution is performed on behalf of
   * \param path  The path being resolved
   * \param flags The access mode requested
   * \param symlink_limit Don't follow symlinks deeper than this number of levels
   * \returns a resolution result, which is either an artifact or an error code
   */
  Ref resolve(const shared_ptr<Command>& c,
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
  virtual Ref resolve(const shared_ptr<Command>& c,
                      shared_ptr<Artifact> prev,
                      fs::path::iterator current,
                      fs::path::iterator end,
                      AccessFlags flags,
                      size_t symlink_limit) noexcept;

  /****** Utility Methods ******/

  /// Print this artifact
  friend ostream& operator<<(ostream& o, const Artifact& a) noexcept {
    o << "[" << a.getTypeName();
    auto name = a.getName();
    if (!name.empty()) o << " " << name;
    o << "]";
    return o;
  }

  /// Print a pointer to an artifact
  friend ostream& operator<<(ostream& o, const Artifact* a) noexcept {
    if (a == nullptr) return o << "<null Artifact>";
    return o << *a;
  }

 protected:
  /// Add a metadata version to this artifact's list of versions
  void appendVersion(shared_ptr<MetadataVersion> v) noexcept;

  /// Add a content version to this artifact's list of versions
  void appendVersion(shared_ptr<ContentVersion> v) noexcept;

 protected:
  /// The environment this artifact is managed by
  weak_ptr<Env> _env;

  /// The latest metadata version
  shared_ptr<MetadataVersion> _metadata_version;

  /// A link update records a directory, entry name, and the version that creates/removes the link
  using LinkUpdate = tuple<weak_ptr<DirArtifact>, fs::path, weak_ptr<DirVersion>>;

  /// Keep track of the sequence of links and unlinks to this artifact
  list<LinkUpdate> _link_updates;

  /// A path to a temporary location where this artifact is stored
  optional<fs::path> _temp_path;

 private:
  /// A fixed string name assigned to this artifact
  string _name;

  /// The sequence of metadata versions applied to this artifact
  list<shared_ptr<MetadataVersion>> _metadata_versions;

  /// The sequence of content versions applied to this artifact
  list<shared_ptr<ContentVersion>> _content_versions;
};
