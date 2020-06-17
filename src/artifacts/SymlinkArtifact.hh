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

class SymlinkArtifact final : public Artifact {
 public:
  SymlinkArtifact(Env& env,
                  bool committed,
                  shared_ptr<MetadataVersion> mv,
                  shared_ptr<SymlinkVersion> sv) noexcept;

  virtual string getTypeName() const noexcept final { return "Symlink"; }

  /// The provided command depends on all current versions of this artifact
  virtual void needsCurrentVersions(shared_ptr<Command> c) noexcept override;

  virtual const shared_ptr<SymlinkVersion>& readlink(shared_ptr<Command> c) noexcept override;

 private:
  /// The currrent version of this symlink
  shared_ptr<SymlinkVersion> _symlink_version;

  // Is the current sylink version committed to the filesystem?
  bool _symlink_committed;
};
