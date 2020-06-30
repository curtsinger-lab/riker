#pragma once

#include <map>
#include <memory>
#include <optional>
#include <string>

#include "artifacts/Artifact.hh"
#include "build/Resolution.hh"
#include "versions/DirVersion.hh"

using std::map;
using std::optional;
using std::shared_ptr;
using std::string;

class Command;
class FileVersion;
class Ref;
class Version;

class DirArtifact final : public Artifact {
 public:
  DirArtifact(Env& env, shared_ptr<MetadataVersion> mv, shared_ptr<DirVersion> dv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "Dir"; }

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

  /************ Directory Operations ************/

  virtual Resolution resolve(shared_ptr<Command> c,
                             shared_ptr<Artifact> prev,
                             fs::path::iterator current,
                             fs::path::iterator end,
                             shared_ptr<Access> ref,
                             bool committed) noexcept override;

  /// Apply a link version to this artifact
  virtual void apply(shared_ptr<Command> c, shared_ptr<AddEntry> writing) noexcept override;

  /// Apply an unlink version to this artifact
  virtual void apply(shared_ptr<Command> c, shared_ptr<RemoveEntry> writing) noexcept override;

 private:
  /// The list of versions of this directory, from newest to oldest
  list<shared_ptr<DirVersion>> _dir_versions;
};