#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <string>

#include "artifacts/Artifact.hh"

using std::map;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

class RefResult;

class SymlinkArtifact : public Artifact {
 public:
  SymlinkArtifact(shared_ptr<Env> env,
                  shared_ptr<MetadataVersion> mv,
                  shared_ptr<SymlinkVersion> sv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get a printable name for this artifact type
  virtual string getTypeName() const noexcept override { return "Symlink"; }

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

  /************ Symlink Operations ************/

  /// Get the current symlink version of this artifact
  virtual shared_ptr<Version> getContent(Build& build,
                                         shared_ptr<Command> c,
                                         InputType t) noexcept override;

  virtual Resolution resolve(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<Artifact> prev,
                             fs::path::iterator current,
                             fs::path::iterator end,
                             AccessFlags flags,
                             bool committed) noexcept override;

 private:
  /// The currrent version of this symlink
  shared_ptr<SymlinkVersion> _symlink_version;
};
