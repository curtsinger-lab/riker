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
  DirArtifact(shared_ptr<Env> env,
              shared_ptr<MetadataVersion> mv,
              shared_ptr<BaseDirVersion> dv) noexcept;

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

  /// Command c requires that this artifact exists in its current state. Create dependency edges.
  virtual void mustExist(Build& build, shared_ptr<Command> c) noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(Build& build, fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

  /************ Directory Operations ************/

  /// Get a version that lists all the entries in this directory
  virtual shared_ptr<Version> getContent(Build& build,
                                         shared_ptr<Command> c,
                                         InputType t) noexcept override;

  virtual Resolution resolve(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<Artifact> prev,
                             fs::path::iterator current,
                             fs::path::iterator end,
                             shared_ptr<Access> ref,
                             bool committed) noexcept override;

  /// Apply a link version to this artifact
  virtual void apply(Build& build,
                     shared_ptr<Command> c,
                     shared_ptr<AddEntry> writing) noexcept override;

  /// Apply an unlink version to this artifact
  virtual void apply(Build& build,
                     shared_ptr<Command> c,
                     shared_ptr<RemoveEntry> writing) noexcept override;

 private:
  /// The base directory version is the backstop for all resolution queries. This is either an
  /// on-disk verison, or an empty directory
  shared_ptr<BaseDirVersion> _base_dir_version;

  /// A map of entries in this directory. Each mapped value is a pair of the version that created
  /// that entry and the artifact that entry resolves to (or nullptr if the entry is absent).
  map<string, tuple<shared_ptr<DirVersion>, shared_ptr<Artifact>>> _entries;
};