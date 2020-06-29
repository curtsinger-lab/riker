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
class Reference;
class Version;

class DirArtifact final : public Artifact {
 public:
  DirArtifact(Env& env, shared_ptr<MetadataVersion> mv, shared_ptr<DirVersion> dv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "Dir"; }

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept override;

  /// Can this artifact be fully committed?
  virtual bool canCommit() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commit() noexcept override;

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
  virtual void apply(shared_ptr<Command> c, shared_ptr<LinkVersion> writing) noexcept override;

  /// Apply an unlink version to this artifact
  virtual void apply(shared_ptr<Command> c, shared_ptr<UnlinkVersion> writing) noexcept override;

 private:
  /// The list of versions of this directory, from newest to oldest
  list<shared_ptr<DirVersion>> _dir_versions;
};