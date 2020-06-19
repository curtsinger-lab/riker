#pragma once

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

  virtual string getTypeName() const noexcept override { return "Dir"; }

  virtual void finalize(shared_ptr<Reference> ref) noexcept override;

  /**
   * Attempt to access a directory entry in the current artifact.
   * \param c     The command making the access
   * \param ref   A reference that was used to reach this directory
   * \param entry The name of the entry being requested
   * \returns a resolution result, holding either an artifact or error code
   */
  virtual Resolution getEntry(shared_ptr<Command> c,
                              shared_ptr<Access> ref,
                              string entry) noexcept override;

  /**
   * Add an entry to this directory
   * \param c      The command making the access
   * \param ref    A reference that was used to reach this directory
   * \param entry  The name of the directory entry
   * \param target A reference to the artifact that is being linked into the directory
   */
  virtual void addEntry(shared_ptr<Command> c,
                        shared_ptr<Access> ref,
                        string entry,
                        shared_ptr<Access> target) noexcept override;

 private:
  map<string, weak_ptr<Artifact>> _resolved;

  list<shared_ptr<DirVersion>> _dir_versions;

  bool _dir_committed;

  bool _finalized = false;
};