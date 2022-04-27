#pragma once

#include <filesystem>
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <tuple>

#include "artifacts/Artifact.hh"
#include "runtime/Ref.hh"

namespace fs = std::filesystem;

class AccessFlags;
class Build;
class Command;
class ContentVersion;
class MetadataVersion;
class PipeReadVersion;
class PipeWriteVersion;

class PipeArtifact : public Artifact {
 public:
  /**
   * Create a new pipe with no existing metadata. This call should be followed by a call to
   * updateMetadata.
   */
  PipeArtifact() noexcept {}

  /**
   * Create a pipe artifact with existing metadata
   */
  PipeArtifact(MetadataVersion mv) noexcept : Artifact(mv) {}

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual std::string getTypeName() const noexcept override { return "Pipe"; }

  /// Commit the content of this artifact to a specific path
  virtual void commitContentTo(fs::path path) noexcept override {}

  /// Does this artifact have any uncommitted content?
  virtual bool hasUncommittedContent() noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path PathRef) noexcept override {}

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override {}

  /// Revert this artifact to its committed state
  virtual void rollback() noexcept override;

  /************ Path Operations ************/

  /// Commit a link to this artifact at the given path
  virtual void commitLink(std::shared_ptr<DirEntry> entry) noexcept override;

  /// Commit an unlink of this artifact at the given path
  virtual void commitUnlink(std::shared_ptr<DirEntry> entry) noexcept override;

  /************ Traced Operations ************/

  /// A traced command is about to close a reference to this artifact
  virtual void beforeClose(Build& build,
                           const IRSource& source,
                           const std::shared_ptr<Command>& c,
                           Ref::ID ref) noexcept override;

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          const IRSource& source,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override {}

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
                          Ref::ID ref) noexcept override {}

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

  /************ Miscellaneous ************/
  /// Get a file descriptor for this artifact
  virtual int getFD(AccessFlags flags) noexcept override;

  /// Set file descriptors for this pipe
  void setFDs(int read_fd, int write_fd) noexcept { _fds = {read_fd, write_fd}; }

 protected:
  /// Skip committing metadata to pipes
  virtual void commitMetadata() noexcept override { _metadata.setCommitted(); }

  /// Skip committing metadata to pipes
  virtual void commitMetadataTo(fs::path path) noexcept override { _metadata.setCommitted(); }

 private:
  /// The last command to read from this pipe
  std::weak_ptr<Command> _last_reader;

  /// The most recent read from this pipe
  std::shared_ptr<PipeReadVersion> _last_read;

  /// A list of writes to this pipe. Each is a pair of the written version and writer
  std::list<std::tuple<std::shared_ptr<PipeWriteVersion>, std::weak_ptr<Command>>> _writes;

  /// Is this pipe running in committed or uncommitted mode? Unlike other artifacts, once a pipe
  /// takes on a committed or uncommitted state it does not change.
  std::optional<bool> _committed_mode;

  /// File descriptors for an actual opened pipe
  std::optional<std::tuple<int, int>> _fds;
};