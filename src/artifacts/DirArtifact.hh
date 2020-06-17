#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "build/Resolution.hh"

using std::map;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

namespace fs = std::filesystem;

class Command;
class Reference;
class Version;
class ContentVersion;

class DirArtifact final : public Artifact {
 public:
  DirArtifact(Env& env,
              bool committed,
              shared_ptr<MetadataVersion> mv = make_shared<MetadataVersion>()) noexcept :
      Artifact(env, committed, mv) {}

  virtual string getTypeName() const noexcept final { return "Dir"; }

  virtual void finalize(shared_ptr<Reference> ref) noexcept final;

  /**
   * Attempt to access a directory entry in the current artifact.
   * \param self  The path that was used to reach this directory
   * \param entry The name of the entry being requested
   * \returns a resolution result, either an artifact or an error code
   */
  virtual Resolution getEntry(fs::path self, string entry) noexcept final;

  virtual void setEntry(string entry, shared_ptr<Artifact> target) noexcept final;

 private:
  map<string, weak_ptr<Artifact>> _entries;
  bool _finalized = false;
};