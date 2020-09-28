#pragma once

#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "interfaces/BuildObserver.hh"

using std::shared_ptr;
using std::string;

class Command;
class FileVersion;
class Ref;
class Version;

class FileArtifact : public Artifact {
 public:
  FileArtifact(shared_ptr<Env> env,
               shared_ptr<MetadataVersion> mv,
               shared_ptr<FileVersion> cv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "File"; }

  /// Can a specific version of this artifact be committed?
  virtual bool canCommit(shared_ptr<Version> v) const noexcept override;

  /// Commit a specific version of this artifact to the filesystem
  virtual void commit(shared_ptr<Version> v) noexcept override;

  /// Can this artifact be fully committed?
  virtual bool canCommitAll() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commitAll() noexcept override;

  /// Command c requires that this artifact exists in its current state. Create dependency edges.
  virtual void mustExist(Build& build, shared_ptr<Command> c) noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(Build& build, fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

  /// Mark all versions of this artifact as committed
  virtual void setCommitted() noexcept override;

  /************ Content Operations ************/

  /// Get the current content version for this artifact
  virtual shared_ptr<Version> getContent(Build& build,
                                         shared_ptr<Command> c,
                                         InputType t) noexcept override;

  /// Create a new version to track updated contents for this artifact
  virtual shared_ptr<Version> createContentVersion() noexcept override;

  /// Apply a new content version to this artifact
  virtual void updateContent(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<FileVersion> writing) noexcept override;

 private:
  /// The latest content version
  shared_ptr<FileVersion> _content_version;
};