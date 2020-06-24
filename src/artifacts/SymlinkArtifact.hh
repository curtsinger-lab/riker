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
  SymlinkArtifact(Env& env,
                  bool committed,
                  shared_ptr<MetadataVersion> mv,
                  shared_ptr<SymlinkVersion> sv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get a printable name for this artifact type
  virtual string getTypeName() const noexcept override { return "Symlink"; }

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept override { return _symlink_committed; }

  /// Do we have saved copies of all versions in this artifact?
  virtual bool isSaved() const noexcept override { return true; }

  /// Commit any un-committed version of this artifact using the provided reference
  virtual void commit(shared_ptr<Reference> ref) noexcept override;

  /// Check the final state of this artifact and save any necessary final fingerprints
  virtual void finalize(shared_ptr<Reference> ref, bool commit) noexcept override;

  /// A command depends on all current versions of this artifact
  virtual void needsCurrentVersions(shared_ptr<Command> c) noexcept override;

  /************ Symlink Operations ************/

  /// Get the current symlink version of this artifact
  virtual shared_ptr<SymlinkVersion> getSymlink(shared_ptr<Command> c,
                                                InputType t) noexcept override;

  /// Check to see if this artifact's symlink destination matches a known version
  virtual void match(shared_ptr<Command> c, shared_ptr<SymlinkVersion> expected) noexcept override;

 private:
  /// The currrent version of this symlink
  shared_ptr<SymlinkVersion> _symlink_version;

  // Is the current sylink version committed to the filesystem?
  bool _symlink_committed;
};
