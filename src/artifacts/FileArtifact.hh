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
  FileArtifact(Env& env, shared_ptr<MetadataVersion> mv, shared_ptr<ContentVersion> cv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "File"; }

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept override;

  /// Can this artifact be fully committed?
  virtual bool canCommit() const noexcept override;

  /// Commit any un-committed version of this artifact using the provided reference
  virtual void commit(shared_ptr<Reference> ref) noexcept override;

  /// Check the final state of this artifact and save any necessary final fingerprints
  virtual void finalize(shared_ptr<Reference> ref, bool commit) noexcept override;

  /// The provided command depends on all current versions of this artifact
  virtual void needsCurrentVersions(shared_ptr<Command> c) noexcept override;

  /************ Content Operations ************/

  /// Get the current content version for this artifact
  virtual shared_ptr<ContentVersion> getContent(shared_ptr<Command> c,
                                                InputType t) noexcept override;

  /// Check to see if this artifact's content matches a known version
  virtual void match(shared_ptr<Command> c, shared_ptr<ContentVersion> expected) noexcept override;

  /// Apply a new content version to this artifact
  virtual void apply(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<ContentVersion> writing) noexcept override;

 private:
  /// The latest content version
  shared_ptr<ContentVersion> _content_version;
};