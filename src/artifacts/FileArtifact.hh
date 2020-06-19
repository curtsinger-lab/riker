#pragma once

#include <memory>
#include <string>

#include "artifacts/Artifact.hh"

using std::shared_ptr;
using std::string;

class Command;
class ContentVersion;
class Reference;
class Version;

class FileArtifact : public Artifact {
 public:
  FileArtifact(Env& env,
               bool committed,
               shared_ptr<MetadataVersion> mv,
               shared_ptr<ContentVersion> cv) noexcept;

  /// Check the final state of this artifact and save any necessary final fingerprints
  virtual void finalize(shared_ptr<Reference> ref) noexcept override;

  /// Do we have a saved copy of this artifact that can be committed to the filesystem?
  virtual bool isSaved() const noexcept override;

  /// Save a copy of this artifact's versions so it can be restored on a future build
  virtual void save(shared_ptr<Reference> ref) noexcept override;

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept override;

  /// Commit any un-committed version of this artifact using the provided reference
  virtual void commit(shared_ptr<Reference> ref) noexcept override;

  /// Do we have a fingerprint of this artifact's versions that will allow us to check for a match?
  virtual bool hasFingerprint() const noexcept override;

  /// Save a fingerprint of this artifact's versions so we can check for a match
  virtual void fingerprint(shared_ptr<Reference> ref) noexcept override;

  virtual const shared_ptr<ContentVersion>& accessContents(
      shared_ptr<Command> c,
      shared_ptr<Reference> ref) noexcept override;

  virtual const shared_ptr<ContentVersion>& setContents(
      shared_ptr<Command> c,
      shared_ptr<Reference> ref,
      shared_ptr<ContentVersion> v = nullptr) noexcept override;

  virtual string getTypeName() const noexcept override { return "File"; }

  /// The provided command depends on all current versions of this artifact
  virtual void needsCurrentVersions(shared_ptr<Command> c) noexcept override;

 private:
  /// The latest content version
  shared_ptr<ContentVersion> _content_version;

  /// Is the latest content version committed?
  bool _content_committed;
};