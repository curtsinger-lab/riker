#pragma once

#include <list>
#include <memory>
#include <optional>
#include <string>
#include <tuple>

#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "util/serializer.hh"
#include "versions/PipeVersion.hh"

using std::list;
using std::nullopt;
using std::optional;
using std::shared_ptr;
using std::string;
using std::tuple;

class Build;
class Command;
class MetadataVersion;
class Ref;

class PipeArtifact : public Artifact {
 public:
  PipeArtifact(shared_ptr<MetadataVersion> mv) noexcept : Artifact(mv) {}

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "Pipe"; }

  /// Can a specific version of this artifact be committed?
  virtual bool canCommit(shared_ptr<ContentVersion> v) const noexcept override;

  /// Commit any metadata updates to the filesystem
  virtual void commitMetadata(optional<fs::path> path = nullopt) noexcept override {}

  /// Commit a specific version of this artifact to the filesystem
  virtual void commit(shared_ptr<ContentVersion> v) noexcept override {}

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll(optional<fs::path> path = nullopt) noexcept override {}

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path PathRef) noexcept override {}

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override {}

  /// Mark all versions of this artifact as committed
  virtual void setCommitted() noexcept override;

  /************ Traced Operations ************/

  /// A traced command is about to close a reference to this artifact
  virtual void beforeClose(Build& build,
                           const shared_ptr<Command>& c,
                           Ref::ID ref) noexcept override;

  /// A traced command is about to (possibly) read from this artifact
  virtual void beforeRead(Build& build,
                          const shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override {}

  /// A traced command just read from this artifact
  virtual void afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept override;

  /// A traced command is about to (possibly) write to this artifact
  virtual void beforeWrite(Build& build,
                           const shared_ptr<Command>& c,
                           Ref::ID ref) noexcept override;

  /// A trace command just wrote to this artifact
  virtual void afterWrite(Build& build,
                          const shared_ptr<Command>& c,
                          Ref::ID ref) noexcept override {}

  /************ Content Operations ************/

  /// Get this artifact's current content without creating any dependencies
  virtual shared_ptr<ContentVersion> peekContent() noexcept override;

  /// Check to see if this artifact's content matches a known version
  virtual void matchContent(const shared_ptr<Command>& c,
                            Scenario scenario,
                            shared_ptr<ContentVersion> expected) noexcept override;

  /// Apply a new content version to this artifact
  virtual void updateContent(const shared_ptr<Command>& c,
                             shared_ptr<ContentVersion> writing) noexcept override;

  /************ Miscellaneous ************/
  /// Get a file descriptor for this artifact
  virtual int getFD(AccessFlags flags) noexcept override;

  /// Set file descriptors for this pipe
  void setFDs(int read_fd, int write_fd) noexcept { _fds = {read_fd, write_fd}; }

 private:
  /// The version that tracks the most recent read from this pipe
  shared_ptr<PipeReadVersion> _last_read = nullptr;

  /// The versions that have been written to this pipe since the last read
  list<shared_ptr<PipeWriteVersion>> _writes;

  // File descriptors for an actual opened pipe
  optional<tuple<int, int>> _fds;
};