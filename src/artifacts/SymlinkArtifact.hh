#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "core/IR.hh"

using std::map;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

class SymlinkArtifact : public Artifact {
 public:
  SymlinkArtifact(Env& env, shared_ptr<MetadataVersion> mv, shared_ptr<SymlinkVersion> sv) noexcept;

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

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState() noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState() noexcept override;

  /************ Symlink Operations ************/

  /// Get the current symlink version of this artifact
  virtual shared_ptr<SymlinkVersion> getSymlink(shared_ptr<Command> c,
                                                InputType t) noexcept override;

  /// Check to see if this artifact's symlink destination matches a known version
  virtual void match(shared_ptr<Command> c, shared_ptr<SymlinkVersion> expected) noexcept override;

  virtual Resolution resolve(shared_ptr<Command> c,
                             shared_ptr<Artifact> prev,
                             fs::path::iterator current,
                             fs::path::iterator end,
                             shared_ptr<Access> ref,
                             bool committed) noexcept override;

 private:
  /// The currrent version of this symlink
  shared_ptr<SymlinkVersion> _symlink_version;
};
