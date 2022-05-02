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
class MetadataVersion;
class SpecialVersion;

class SpecialArtifact : public Artifact {
 public:
  /**
   * Create a new SpecialArtifact with existing metadata. The artifact will either appear to be
   * changed or matching another version depending on how the artifact is initialized. This artifact
   * type is useful for special devices like /dev/urandom or /dev/tty. Specific handling for special
   * devices is specified in the implementation of env::getFilesystemArtifact().
   */
  SpecialArtifact(MetadataVersion mv, bool always_changed) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual std::string getTypeName() const noexcept override { return "Special"; }

  /// Commit the content of this artifact to a specific path
  virtual void commitContentTo(fs::path path) noexcept override;

  /// Does this artifact have any uncommitted content?
  virtual bool hasUncommittedContent() noexcept override { return false; }

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

  /// Revert this artifact to its committed state
  virtual void rollback() noexcept override;

  /// Get a file descriptor for this artifact
  virtual int getFD(AccessFlags flags) noexcept override;

  /// Set the file descriptor for this artifact
  void setFD(int fd) noexcept;

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

 private:
  /// The file descriptor for this artifact, if any
  int _fd = -1;

  /// Do comparisons against this always report a change?
  bool _always_changed;

  /// The content of this artifact
  VersionState<SpecialVersion> _content;
};