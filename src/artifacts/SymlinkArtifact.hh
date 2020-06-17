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
                  fs::path dest) noexcept :
      Artifact(env, committed, mv), _dest(dest) {}

  virtual string getTypeName() const noexcept final { return "Symlink"; }

  fs::path readlink() const noexcept { return _dest; }

 private:
  fs::path _dest;
};