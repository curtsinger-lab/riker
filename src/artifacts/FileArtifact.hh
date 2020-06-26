#pragma once

#include <memory>
#include <string>

#include "artifacts/Artifact.hh"

using std::shared_ptr;
using std::string;

class Command;
class FileVersion;
class Reference;
class Version;

class FileArtifact : public Artifact {
 public:
  FileArtifact(Env& env, shared_ptr<MetadataVersion> mv, shared_ptr<FileVersion> cv) noexcept;

  /************ Core Artifact Operations ************/

  /// Get the name of this artifact type
  virtual string getTypeName() const noexcept override { return "File"; }

  /// Have all modifications to this artifact been committed to the filesystem?
  virtual bool isCommitted() const noexcept override;

  /// Can this artifact be fully committed?
  virtual bool canCommit() const noexcept override;

  /// Commit all final versions of this artifact to the filesystem
  virtual void commit(fs::path path) noexcept override;

  /// Compare all final versions of this artifact to the filesystem state
  virtual void checkFinalState(fs::path path) noexcept override;

  /// Commit any pending versions and save fingerprints for this artifact
  virtual void applyFinalState(fs::path path) noexcept override;

  /************ Content Operations ************/

  /// Get the current content version for this artifact
  virtual shared_ptr<FileVersion> getContent(shared_ptr<Command> c,
                                             shared_ptr<Reference> ref,
                                             InputType t) noexcept override;

  /// Check to see if this artifact's content matches a known version
  virtual void match(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<FileVersion> expected) noexcept override;

  /// Apply a new content version to this artifact
  virtual void apply(shared_ptr<Command> c,
                     shared_ptr<Reference> ref,
                     shared_ptr<FileVersion> writing) noexcept override;

 private:
  /// The latest content version
  shared_ptr<FileVersion> _content_version;
};