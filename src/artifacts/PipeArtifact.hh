#pragma once

#include <list>
#include <memory>
#include <string>
#include <tuple>

#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::list;
using std::shared_ptr;
using std::string;
using std::tuple;

class Build;
class Command;
class Ref;

class PipeWriteVersion : public Version {
 public:
  /// Create a new version to track a write to a pipe
  PipeWriteVersion() noexcept : Version() {}

  /// Get a short name for this version type
  virtual string getTypeName() const noexcept override { return "pipe write"; }

  /// Check if a written pipe version matches another
  virtual bool matches(shared_ptr<Version> other) const noexcept override;

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[pipe write]"; }

 private:
  SERIALIZE(BASE(Version));
};

class PipeCloseVersion : public PipeWriteVersion {
 public:
  /// Get a short name for thsi version type
  virtual string getTypeName() const noexcept override { return "pipe close"; }

  /// Check if a this version matches another
  virtual bool matches(shared_ptr<Version> other) const noexcept override {
    return static_cast<bool>(other->as<PipeCloseVersion>());
  }

  /// Pipe closes can always be committed
  virtual bool isSaved() const noexcept override { return true; }

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept override { return o << "[pipe close]"; }

 private:
  SERIALIZE(BASE(PipeWriteVersion));
};

class PipeReadVersion : public Version {
 public:
  /// Create a new version that tracks a read from a pipe. The read observes some number of writes.
  PipeReadVersion(list<shared_ptr<PipeWriteVersion>> observed) noexcept : _observed(observed) {}

  /// Get a short name for this version type
  virtual string getTypeName() const noexcept override { return "pipe read"; }

  /// Check if a read pipe version matches another
  virtual bool matches(shared_ptr<Version> other) const noexcept override;

  /// Print this version
  virtual ostream& print(ostream& o) const noexcept override {
    return o << "[pipe read " << _observed.size() << "]";
  }

 private:
  /// The list of writes this read version observes
  list<shared_ptr<PipeWriteVersion>> _observed;

  // Create default constructor and declare fields for serialization
  PipeReadVersion() noexcept = default;
  SERIALIZE(BASE(Version), _observed);
};

class PipeArtifact : public Artifact {
 public:
  PipeArtifact(shared_ptr<Env> env, shared_ptr<MetadataVersion> mv) noexcept : Artifact(env, mv) {}

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
  virtual void mustExist(Build& build, const shared_ptr<Command>& c) noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(Build& build, fs::path path) noexcept override {}

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(Build& build, fs::path path) noexcept override {}

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
  virtual shared_ptr<Version> peekContent() noexcept override;

  /// Check to see if this artifact's content matches a known version
  virtual void matchContent(Build& build,
                            const shared_ptr<Command>& c,
                            Scenario scenario,
                            shared_ptr<Version> expected) noexcept override;

  /// Apply a new content version to this artifact
  virtual void updateContent(Build& build,
                             const shared_ptr<Command>& c,
                             shared_ptr<Version> writing) noexcept override;

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