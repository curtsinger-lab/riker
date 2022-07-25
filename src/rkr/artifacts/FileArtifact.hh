#pragma once

#include <filesystem>
#include <memory>
#include <optional>
#include <string>

#include "artifacts/Artifact.hh"
#include "runtime/Ref.hh"
#include "runtime/VersionState.hh"

namespace fs = std::filesystem;

class Build;
class ContentVersion;
class Command;
class FileVersion;
class MetadataVersion;

class FileArtifact : public Artifact {
 public:
  /**
   * Create a new FileArtifact with no initial metadata or content. This should be followed by calls
   * to updateMetadata and updateContent to fully initialize the artifact.
   */
  FileArtifact() noexcept : Artifact() {}

  /**
   * Create a new FileArtifact with existing metadata and content versions. This only appropriate
   * for artifacts that exist on the filesystem.
   */
  FileArtifact(MetadataVersion mv, std::shared_ptr<FileVersion> cv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual std::string getTypeName() const noexcept override { return "File"; }

  /// Commit the content of this artifact to a specific path
  virtual void commitContentTo(fs::path path) noexcept override;

  /// Does this artifact have any uncommitted content?
  virtual bool hasUncommittedContent() noexcept override { return !_content.isCommitted(); }

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

  /// A traced command is about to stat this artifact
  virtual void beforeStat(Build& build,
                          const IRSource& source,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override;

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

  /// A traced command is about to (possibly) write to this artifact
  virtual void beforeWrite(Build& build,
                           const IRSource& source,
                           const std::shared_ptr<Command>& c,
                           Ref::ID ref) noexcept override;

  /// A trace command just wrote to this artifact
  virtual void afterWrite(Build& build,
                          const IRSource& source,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override;

  /// A traced command is about to (possibly) truncate this artifact to length zero
  virtual void beforeTruncate(Build& build,
                              const IRSource& source,
                              const std::shared_ptr<Command>& c,
                              Ref::ID ref) noexcept override;

  /// A trace command just truncated this artifact to length zero
  virtual void afterTruncate(Build& build,
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

  /// Apply a new content version to this artifact
  virtual void updateContent(const std::shared_ptr<Command>& c,
                             std::shared_ptr<ContentVersion> writing) noexcept override;

 protected:
  /// Cache and fingerprint this file's content if necessary
  void fingerprintAndCache(const std::shared_ptr<Command>& reader) const noexcept;

 private:
  /// The committed and uncommitted state that represent this file's content
  VersionState<FileVersion> _content;
};

template <>
struct fmt::formatter<FileArtifact> : ostream_formatter {};
