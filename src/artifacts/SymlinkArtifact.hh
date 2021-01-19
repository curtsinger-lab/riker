#pragma once

#include <cstddef>
#include <filesystem>
#include <memory>
#include <optional>
#include <string>

#include "artifacts/Artifact.hh"
#include "runtime/CommandRun.hh"
#include "runtime/Ref.hh"

namespace fs = std::filesystem;

class AccessFlags;
class Build;
class Command;
class ContentVersion;
class MetadataVersion;
class SymlinkVersion;

class SymlinkArtifact : public Artifact {
 public:
  /**
   * Create a new SymlinkArtifact with no initial state. This is useful for modeled artifacts that
   * do not exist on the filesystem. This constructor should be followed-up with calls to
   * updateMetadata and updateContent to set the initial state.
   */
  SymlinkArtifact() noexcept : Artifact() {}

  /**
   * Create a SymlinkArtifact that represents an existing symlink on the filesystem.
   */
  SymlinkArtifact(std::shared_ptr<MetadataVersion> mv, std::shared_ptr<SymlinkVersion> sv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get a printable name for this artifact type
  virtual std::string getTypeName() const noexcept override { return "Symlink"; }

  /// Can a specific version of this artifact be committed?
  virtual bool canCommit(std::shared_ptr<ContentVersion> v) const noexcept override;

  /// Commit a specific version of this artifact to the filesystem
  virtual void commit(std::shared_ptr<ContentVersion> v) noexcept override;

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll(std::optional<fs::path> path = std::nullopt) noexcept override;

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

  /// Set the content of this symlink
  virtual void updateContent(const std::shared_ptr<Command>& c,
                             std::shared_ptr<ContentVersion> writing) noexcept override;

  /************ Symlink Operations ************/

  virtual Ref resolve(const std::shared_ptr<Command>& c,
                      std::shared_ptr<Artifact> prev,
                      fs::path::iterator current,
                      fs::path::iterator end,
                      AccessFlags flags,
                      size_t symlink_limit) noexcept override;

 protected:
  /// Skip committing metadata to symlinks
  virtual void commitMetadata(std::optional<fs::path> path = std::nullopt) noexcept override {
    Artifact::setMetadataCommitted();
  }

 private:
  /// The command that most recently wrote this artifact's content, possibly null
  std::weak_ptr<Command> _content_writer;

  /// The current uncommitted content, if any
  std::shared_ptr<SymlinkVersion> _uncommitted_content;

  /// The on-filesystem version of this artifact's content
  std::shared_ptr<SymlinkVersion> _committed_content;
};
