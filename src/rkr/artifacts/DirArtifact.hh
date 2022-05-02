#pragma once

#include <cstddef>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <tuple>

#include "artifacts/Artifact.hh"
#include "runtime/Ref.hh"
#include "runtime/VersionState.hh"

namespace fs = std::filesystem;

class AccessFlags;
class BaseDirVersion;
class Build;
class Command;
class ContentVersion;
class DirEntry;
class DirListVersion;
class DirVersion;
class DirEntryVersion;
class MetadataVersion;

class DirArtifact final : public Artifact {
 public:
  /// Create a DirArtifact that exists only in the filesystem model. This should be followed by
  /// calls to updateMetadata and createEmptyDir
  DirArtifact() noexcept = default;

  /// Create a DirArtifact with existing committed metadata and content
  DirArtifact(MetadataVersion mv, std::shared_ptr<BaseDirVersion> dv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual std::string getTypeName() const noexcept override { return "Dir"; }

  /// Commit the content of this artifact to a specific path
  virtual void commitContentTo(fs::path path) noexcept override;

  /// Does this artifact have any uncommitted content?
  virtual bool hasUncommittedContent() noexcept override;

  /// Commit all entries in this directory
  void commitAll() noexcept;

  /// Commit a specific entry in this directory
  void commitEntry(std::string name) noexcept;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

  /// Fingerprint and cache the committed state of this artifact
  virtual void cacheAll(fs::path path) const noexcept override;

  /// Revert this artifact to its committed state
  virtual void rollback() noexcept override;

  /************ Path Operations ************/

  /// Commit a link to this artifact at the given path
  virtual void commitLink(std::shared_ptr<DirEntry> entry) noexcept override;

  /// Commit an unlink of this artifact at the given path
  virtual void commitUnlink(std::shared_ptr<DirEntry> entry) noexcept override;

  /************ Traced Operations ************/

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          const IRSource& source,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override;

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build,
                         const IRSource& source,
                         const std::shared_ptr<Command>& c,
                         Ref::ID ref) noexcept override;

  /************ Content Operations ************/

  /// Get this artifact's current content
  virtual std::shared_ptr<ContentVersion> getContent(
      const std::shared_ptr<Command>& c) noexcept override;

  /// Check to see if this artifact's content matches a known version
  virtual void matchContent(const std::shared_ptr<Command>& c,
                            Scenario scenario,
                            std::shared_ptr<ContentVersion> expected) noexcept override;

  /************ Directory Operations ************/

  /// Initialize this directory as an empty dir created by command c
  void createEmptyDir(std::shared_ptr<Command> c) noexcept;

  /// Add a directory entry to this artifact
  virtual void addEntry(const std::shared_ptr<Command>& c,
                        std::string name,
                        std::shared_ptr<Artifact> target) noexcept override;

  /// Remove a directory entry from this artifact
  virtual void removeEntry(const std::shared_ptr<Command>& c,
                           std::string name,
                           std::shared_ptr<Artifact> target) noexcept override;

  // Un-hide the shorthand version of resolve()
  using Artifact::resolve;

  virtual Ref resolve(const std::shared_ptr<Command>& c,
                      std::shared_ptr<Artifact> prev,
                      fs::path::iterator current,
                      fs::path::iterator end,
                      AccessFlags flags,
                      size_t symlink_limit) noexcept override;

 private:
  /// A map of entries in this directory
  std::map<std::string, std::shared_ptr<DirEntry>> _entries;

  /// The base directory content is the backstop for all resolution queries
  VersionState<BaseDirVersion> _base;
};

class DirEntry : public std::enable_shared_from_this<DirEntry> {
 public:
  /**
   * Create an entry with no initial target or version
   *
   * \param dir   The directory that contains this entry
   * \param name  The name of this entry in the containing directory
   */
  DirEntry(std::shared_ptr<DirArtifact> dir, std::string name) noexcept;

  // Disallow copying
  DirEntry(const DirEntry&) = delete;
  DirEntry& operator=(const DirEntry&) = delete;

  // Allow moving
  DirEntry(DirEntry&&) = default;
  DirEntry& operator=(DirEntry&&) = default;

  /**
   * Set the committed state for this entry. Only used for initial state
   *
   * \param target  The artifact this entry links to (possibly nullptr)
   * \param version The version responsible for this entry's state
   */
  void setCommittedState(std::shared_ptr<DirEntryVersion> version) noexcept;

  /// Commit this entry's modeled state to the filesystem
  void commit() noexcept;

  /// Reset this entry to its committed state
  void rollback() noexcept;

  /**
   * Update this entry to reach a new target artifact on behalf of a command
   *
   * \param c       The command updating this entry
   * \param version The latest version to apply to this entry
   */
  void updateEntry(std::shared_ptr<Command> c, std::shared_ptr<DirEntryVersion> version) noexcept;

  /// Peek at the target of this entry without creating a dependency
  std::shared_ptr<Artifact> peekTarget() const noexcept;

  /**
   * Get the artifact linked at this entry on behalf of command c
   *
   * \param c   The command reading the entry
   * \returns   The artifact reachable through this entry, or nullptr if it has been unlinked
   */
  std::shared_ptr<Artifact> getTarget(std::shared_ptr<Command> c) const noexcept;

  /// Get the directory this entry appears in
  std::shared_ptr<DirArtifact> getDir() const noexcept { return _dir.lock(); }

  /// Get the name of this entry in its containing directory
  std::string getName() const noexcept { return _name; }

 private:
  /// The directory that contains this entry
  std::weak_ptr<DirArtifact> _dir;

  /// The name of this entry in the containing directory
  std::string _name;

  /// The committed and uncommitted state of this entry
  VersionState<DirEntryVersion> _state;
};
