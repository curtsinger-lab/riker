#pragma once

#include <cstddef>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <tuple>

#include "artifacts/Artifact.hh"
#include "runtime/CommandRun.hh"
#include "runtime/Ref.hh"

namespace fs = std::filesystem;

class AccessFlags;
class BaseDirVersion;
class Build;
class Command;
class ContentVersion;
class DirListVersion;
class DirVersion;
class MetadataVersion;

class DirArtifact final : public Artifact {
 public:
  /// Create a DirArtifact with no initial metadata version.
  DirArtifact(std::shared_ptr<BaseDirVersion> dv) noexcept;

  /// Create a DirArtifact with existing committed metadata and content
  DirArtifact(std::shared_ptr<MetadataVersion> mv, std::shared_ptr<BaseDirVersion> dv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual std::string getTypeName() const noexcept override { return "Dir"; }

  /// Can a specific version of this artifact be committed?
  virtual bool canCommit(std::shared_ptr<ContentVersion> v) const noexcept override;

  /// Commit a specific version of this artifact to the filesystem
  virtual void commit(std::shared_ptr<ContentVersion> v) noexcept override;

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll(std::optional<fs::path> path = std::nullopt) noexcept override;

  /// Commit the minimal set of versions required to ensure this artifact exists on the filesystem
  virtual void commitMinimal(fs::path path) noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

  /************ Traced Operations ************/

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override;

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build,
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

  /// Add a directory entry to this artifact
  virtual void addEntry(const std::shared_ptr<Command>& c,
                        fs::path entry,
                        std::shared_ptr<Artifact> target) noexcept override;

  /// Remove a directory entry from this artifact
  virtual void removeEntry(const std::shared_ptr<Command>& c,
                           fs::path entry,
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
  /// The base directory version is the backstop for all resolution queries. This is either an
  /// on-disk verison, or an empty directory
  std::shared_ptr<BaseDirVersion> _base_dir_version;

  /// A map of entries in this directory. Each mapped value is a pair of the version that created
  /// that entry and the artifact that entry resolves to (or nullptr if the entry is absent).
  std::map<std::string, std::tuple<std::shared_ptr<DirVersion>, std::shared_ptr<Artifact>>>
      _entries;
};