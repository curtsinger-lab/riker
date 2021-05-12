#pragma once

#include <cstddef>
#include <filesystem>
#include <memory>
#include <optional>
#include <string>

#include "artifacts/Artifact.hh"
#include "runtime/Ref.hh"
#include "runtime/VersionState.hh"

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
  SymlinkArtifact(MetadataVersion mv, std::shared_ptr<SymlinkVersion> sv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get a printable name for this artifact type
  virtual std::string getTypeName() const noexcept override { return "Symlink"; }

  /// Commit the content of this artifact to a specific path
  virtual void commitContentTo(fs::path path) noexcept override;

  /// Does this artifact have any uncommitted content?
  virtual bool hasUncommittedContent() noexcept override { return _content.isUncommitted(); }

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

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
  virtual void commitMetadataTo(fs::path path) noexcept override {
    Artifact::setMetadataCommitted();
  }

 private:
  /// The content of this artifact, both committed and uncommitted
  VersionState<SymlinkVersion> _content;
};
