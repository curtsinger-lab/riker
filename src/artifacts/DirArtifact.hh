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

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "Dir"; }

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept override { return _uncommitted_versions.empty(); }

  /// Do we have saved copies of all versions in this artifact?
  virtual bool isSaved() const noexcept override;

  /// Commit any un-committed version of this artifact using the provided reference
  virtual void commit(shared_ptr<Reference> ref) noexcept override;

  /// Check the final state of this artifact and save any necessary final fingerprints
  virtual void finalize(shared_ptr<Reference> ref, bool commit) noexcept override;

  /// A command depends on all current versions of this artifact
  virtual void needsCurrentVersions(shared_ptr<Command> c) noexcept override;

  /************ Directory Operations ************/

  /**
   * Attempt to access a directory entry in the current artifact.
   * \param c     The command making the access
   * \param ref   A reference that was used to reach this directory
   * \param entry The name of the entry being requested
   * \returns a resolution result, holding either an artifact or error code
   */
  virtual Resolution getEntry(shared_ptr<Command> c,
                              shared_ptr<Reference> ref,
                              string entry) noexcept override;

  /// Apply a link version to this artifact
  virtual void apply(shared_ptr<Command> c,
                     shared_ptr<LinkVersion> writing,
                     bool committed) noexcept override;

  /// Apply an unlink version to this artifact
  virtual void apply(shared_ptr<Command> c,
                     shared_ptr<UnlinkVersion> writing,
                     bool committed) noexcept override;

 private:
  map<string, weak_ptr<Artifact>> _resolved;

  list<shared_ptr<DirVersion>> _uncommitted_versions;

  list<shared_ptr<DirVersion>> _committed_versions;

  bool _finalized = false;
};