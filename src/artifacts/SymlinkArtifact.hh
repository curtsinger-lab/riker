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

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept override;

  /// Can this artifact be fully committed?
  virtual bool canCommit() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commit(fs::path path) noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept override;

  /// Take fingerprints for all final versions of this artifact
  virtual void fingerprintFinalState(fs::path path) noexcept override;

  /************ Symlink Operations ************/

  /// Get the current symlink version of this artifact
  virtual shared_ptr<SymlinkVersion> getSymlink(shared_ptr<Command> c,
                                                shared_ptr<Reference> ref,
                                                InputType t) noexcept override;

  /// Check to see if this artifact's symlink destination matches a known version
  virtual void match(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<SymlinkVersion> expected) noexcept override;

  virtual Resolution resolve(shared_ptr<Command> c,
                             fs::path resolved,
                             fs::path remaining,
                             shared_ptr<Access> ref) noexcept override;

 private:
  /// The currrent version of this symlink
  shared_ptr<SymlinkVersion> _symlink_version;
};
