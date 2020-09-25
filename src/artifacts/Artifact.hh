#pragma once

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>

#include "build/BuildObserver.hh"
#include "build/Resolution.hh"
#include "core/AccessFlags.hh"
#include "ui/options.hh"
#include "util/log.hh"

using std::list;
using std::make_shared;
using std::map;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;

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
class RefResult;
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
  size_t getVersionCount() const noexcept { return _versions.size(); }

  /// Get the list of versions of this artifact
  const list<shared_ptr<Version>>& getVersions() const noexcept { return _versions; }

  /// Get the environment this artifact is part of
  shared_ptr<Env> getEnv() const noexcept { return _env; }

  /**
   * Check if an access to this artifact with the provided flags is allowed.
   * \param c     The command that depends on this access check
   * \param flags The flags that encode whether this is a read, write, and/or execute access
   * \returns true if the access is allowed, or false otherwise
   */
  bool checkAccess(Build& build, shared_ptr<Command> c, AccessFlags flags) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept = 0;

  /// Can a specific version of this artifact be committed?
  virtual bool canCommit(shared_ptr<Version> v) const noexcept = 0;

  /// Commit a specific version (and any co-dependent versions) to the filesystem
  virtual void commit(shared_ptr<Version> v) noexcept = 0;

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept = 0;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll() noexcept = 0;

  /// Command c requires that this artifact exists in its current state. Create dependency edges.
  virtual void mustExist(Build& build, shared_ptr<Command> c) noexcept = 0;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(Build& build, fs::path path) noexcept;

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
  shared_ptr<MetadataVersion> getMetadata(Build& build,
                                          shared_ptr<Command> c,
                                          InputType t) noexcept;

  /// Check to see if this artifact's metadata matches a known version
  void matchMetadata(Build& build,
                     shared_ptr<Command> c,
                     shared_ptr<MetadataVersion> expected) noexcept;

  /// Apply a new metadata version to this artifact
  shared_ptr<MetadataVersion> updateMetadata(
      Build& build,
      shared_ptr<Command> c,
      shared_ptr<MetadataVersion> writing = nullptr) noexcept;

  /************ Content Operations ************/

  /// Get the current content version for this artifact
  virtual shared_ptr<Version> getContent(Build& build,
                                         shared_ptr<Command> c,
                                         InputType t) noexcept = 0;

  /// Check to see if this artifact's content matches a known version
  virtual void matchContent(Build& build,
                            shared_ptr<Command> c,
                            shared_ptr<Version> expected) noexcept;

  /**
   * Create a new version to track updated content for this artifact
   * This method does NOT apply the new version to this artifact; it just creates a version that
   * can be used to track updated content.
   */
  virtual shared_ptr<Version> createContentVersion() noexcept {
    FAIL << "Attempted to create default content version for unsupported artifact " << this;
    return nullptr;
  }

  /// Update this artifact's content with a file version
  virtual void updateContent(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<FileVersion> writing) noexcept {
    WARN << c << ": tried to apply a content version to artifact " << this;
  }

  /************ Directory Operations ************/

  /// Add a directory entry to this artifact
  virtual shared_ptr<DirVersion> addEntry(Build& build,
                                          shared_ptr<Command> c,
                                          fs::path entry,
                                          shared_ptr<Artifact> target) noexcept {
    WARN << c << ": tried to add an entry to non-directory artifact " << this;
    return nullptr;
  }

  /// Remove a directory entry from this artifact
  virtual shared_ptr<DirVersion> removeEntry(Build& build,
                                             shared_ptr<Command> c,
                                             fs::path entry,
                                             shared_ptr<Artifact> target) noexcept {
    WARN << c << ": tried to remove an entry from non-directory artifact " << this;
    return nullptr;
  }

  /**
   * Resolve a path relative to this artifact
   * \param build The running build that issued this request
   * \param c     The command this resolution is performed on behalf of
   * \param path  The path being resolved
   * \param flags The access mode requested
   * \param committed If true, an accesses or actions taken during resolution should be committed
   * \returns a resolution result, which is either an artifact or an error code
   */
  Resolution resolve(Build& build,
                     shared_ptr<Command> c,
                     fs::path path,
                     AccessFlags flags,
                     size_t symlink_limit = 40) noexcept {
    return resolve(build, c, nullptr, path.begin(), path.end(), flags, symlink_limit);
  }

  /**
   * Resolve a path relative to this artifact.
   * \param build     The running build that issued this request
   * \param c         The command this resolution is performed on behalf of
   * \param prev      The previously-visited artifact along this path. This won't always be the
   *                    parent directory, since paths can include ".." entries
   * \param current   An iterator to the next part of the path to be resolved
   * \param end       An iterator to the end of the path
   * \param flags     The access mode requested for the final resolved file
   * \param committed If true, any accesses or actions taken during resolution should be committed.
   * \returns a resolution result, which is either an artifact or an error code
   */
  virtual Resolution resolve(Build& build,
                             shared_ptr<Command> c,
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
  /// Add a version to the sequence of versions for this artifact
  void appendVersion(shared_ptr<Version> v) noexcept;

 protected:
  /// The environment this artifact is managed by
  shared_ptr<Env> _env;

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

  /// The sequence of versions of this artifact applied so far
  list<shared_ptr<Version>> _versions;
};
