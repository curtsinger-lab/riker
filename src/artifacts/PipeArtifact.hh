#pragma once

#include <filesystem>
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <tuple>

#include "artifacts/Artifact.hh"
#include "runtime/CommandRun.hh"
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
  PipeArtifact() noexcept;

  PipeArtifact(std::shared_ptr<MetadataVersion> mv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual std::string getTypeName() const noexcept override { return "Pipe"; }

  /// Can a specific version of this artifact be committed?
  virtual bool canCommit(std::shared_ptr<ContentVersion> v) const noexcept override;

  /// Commit a specific version of this artifact to the filesystem
  virtual void commit(std::shared_ptr<ContentVersion> v) noexcept override {}

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll(std::optional<fs::path> path = std::nullopt) noexcept override {}

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path PathRef) noexcept override {}

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override {}

  /************ Traced Operations ************/

  /// A traced command is about to close a reference to this artifact
  virtual void beforeClose(Build& build,
                           const std::shared_ptr<Command>& c,
                           Ref::ID ref) noexcept override;

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          const std::shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override {}

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build,
                         const std::shared_ptr<Command>& c,
                         Ref::ID ref) noexcept override;

  /// A traced command is about to (possibly) write to this artifact
  virtual void beforeWrite(Build& build,
                           const std::shared_ptr<Command>& c,
                           Ref::ID ref) noexcept override;

  /// A trace command just wrote to this artifact
  virtual void afterWrite(Build& build,
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
  virtual void commitMetadata(std::optional<fs::path> path = std::nullopt) noexcept override {
    Artifact::setMetadataCommitted();
  }

 private:
  /// The version that tracks the most recent read from this pipe
  std::shared_ptr<PipeReadVersion> _last_read = nullptr;

  /// The versions that have been written to this pipe since the last read
  std::list<std::shared_ptr<PipeWriteVersion>> _writes;

  /// File descriptors for an actual opened pipe
  std::optional<std::tuple<int, int>> _fds;
};