#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "build/Resolution.hh"
#include "versions/DirVersion.hh"

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
              shared_ptr<MetadataVersion> mv,
              shared_ptr<DirVersion> dv) noexcept;

  virtual string getTypeName() const noexcept final { return "Dir"; }

  virtual void finalize(shared_ptr<Reference> ref) noexcept final;

  /**
   * Attempt to access a directory entry in the current artifact.
   * \param self  The path that was used to reach this directory
   * \param entry The name of the entry being requested
   * \returns a resolution result, either an artifact or an error code
   */
  virtual Resolution getEntry(shared_ptr<Command> c,
                              fs::path dirpath,
                              fs::path entry) noexcept override;

  virtual void addEntry(shared_ptr<Command> c,
                        fs::path self,
                        fs::path entry,
                        shared_ptr<Reference> target) noexcept override;

 private:
  map<fs::path, weak_ptr<Artifact>> _resolved;

  list<shared_ptr<DirVersion>> _dir_versions;

  bool _dir_committed;

  bool _finalized = false;
};