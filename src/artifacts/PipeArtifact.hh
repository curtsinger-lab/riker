#pragma once

#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "versions/FileVersion.hh"

class PipeArtifact : public Artifact {
 public:
  PipeArtifact(shared_ptr<Env> env,
               shared_ptr<MetadataVersion> mv,
               shared_ptr<FileVersion> cv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "Pipe"; }

  /// Can a specific version of this artifact be committed?
  virtual bool canCommit(shared_ptr<Version> v) const noexcept override;

  /// Commit a specific version of this artifact to the filesystem
  virtual void commit(shared_ptr<Version> v) noexcept override {}

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll() noexcept override {}

  /// Command c requires that this artifact exists in its current state. Create dependency edges.
  virtual void mustExist(Build& build, shared_ptr<Command> c) noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(Build& build, fs::path path) noexcept override {}

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(Build& build, fs::path path) noexcept override {}

  /// Mark all versions of this artifact as committed
  virtual void setCommitted() noexcept override;

  /************ Traced Operations ************/

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          shared_ptr<Command> c,
                          shared_ptr<RefResult> ref) noexcept override {}

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build,
                         shared_ptr<Command> c,
                         shared_ptr<RefResult> ref) noexcept override;

  /// A traced command is about to (possibly) write to this artifact
  virtual void beforeWrite(Build& build,
                           shared_ptr<Command> c,
                           shared_ptr<RefResult> ref) noexcept override;

  /// A trace command just wrote to this artifact
  virtual void afterWrite(Build& build,
                          shared_ptr<Command> c,
                          shared_ptr<RefResult> ref) noexcept override;

  /************ Content Operations ************/

  /// Check to see if this artifact's content matches a known version
  virtual void matchContent(Build& build,
                            shared_ptr<Command> c,
                            shared_ptr<Version> expected) noexcept override;

  /// Apply a new content version to this artifact
  virtual void updateContent(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<Version> writing) noexcept override;

  /************ Miscellaneous ************/
  void open() noexcept {
    if (_read_fd == -1 && _write_fd == -1) {
      int pipefds[2];
      int rc = pipe2(pipefds, O_CLOEXEC);
      ASSERT(rc == 0) << "Failed to create pipe";
      _read_fd = pipefds[0];
      _write_fd = pipefds[1];
      LOG(exec) << "Created pipe with read fd " << _read_fd << " and write fd " << _write_fd;
    }
  }

  void setFDs(int read_fd, int write_fd) {
    _read_fd = read_fd;
    _write_fd = write_fd;
  }

  int getWriteFD() noexcept {
    open();
    return _write_fd;
  }

  int getReadFD() noexcept {
    open();
    return _read_fd;
  }

 private:
  // The current version of the pipe's content
  shared_ptr<FileVersion> _content_version;

  // File descriptors for an actual opened pipe
  int _read_fd = -1;
  int _write_fd = -1;
};