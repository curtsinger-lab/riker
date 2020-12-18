#include "FileArtifact.hh"

#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "runtime/Build.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::shared_ptr;
using std::string;

FileArtifact::FileArtifact(shared_ptr<Env> env,
                           shared_ptr<MetadataVersion> mv,
                           shared_ptr<FileVersion> cv) noexcept :
    Artifact(env, mv) {
  appendVersion(cv);
  _content_version = cv;
}

bool FileArtifact::canCommit(shared_ptr<Version> v) const noexcept {
  if (v == _content_version) {
    return _content_version->canCommit();
  } else if (v == _metadata_version) {
    return _metadata_version->canCommit();
  } else {
    FAIL << "Attempted to check committable state for unknown version " << v << " in " << this;
    return false;
  }
}

void FileArtifact::commit(shared_ptr<Version> v) noexcept {
  auto path = getPath();
  ASSERT(path.has_value()) << "File has no path";

  if (v == _content_version) {
    _content_version->commit(path.value());
  } else if (v == _metadata_version) {
    _metadata_version->commitOwnership(path.value(), true);
  } else {
    FAIL << "Attempted to commit unknown version " << v << " in " << this;
  }
}

/// Do we have saved content and metadata for this artifact?
bool FileArtifact::canCommitAll() const noexcept {
  // Can the metadata version be committed?
  if (!_metadata_version->canCommit()) return false;

  return _content_version->canCommit();
}

/// Commit all final versions of this artifact to the filesystem
void FileArtifact::commitAll() noexcept {
  LOG(artifact) << "Committing " << this;

  // we may have already committed this artifact
  if (_content_version->isCommitted() && _metadata_version->isCommitted()) return;

  auto path = getPath();
  ASSERT(path.has_value()) << "File has no path: " << this;

  _content_version->commitEmptyFile(path.value(), _metadata_version->getMode());
  _metadata_version->commitOwnership(path.value(), true);
}

/// Command c requires that this artifact exists in its current state. Create dependency edges.
void FileArtifact::mustExist(const shared_ptr<Command>& c) noexcept {
  c->currentRun()->addInput(shared_from_this(), _metadata_version, InputType::Exists);
  c->currentRun()->addInput(shared_from_this(), _content_version, InputType::Exists);
}

/// Compare all final versions of this artifact to the filesystem state
void FileArtifact::checkFinalState(fs::path path, fs::path cache_dir) noexcept {
  if (!_content_version->isCommitted()) {
    // generate a content fingerprint for the actual file on disk
    auto v = make_shared<FileVersion>();
    v->fingerprint(path, cache_dir);

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_content_version->matches(v)) {
      // Yes. Report the mismatch
      auto creator = _content_version->getCreator();
      if (creator) creator->outputChanged(shared_from_this(), v, _content_version);

    } else {
      // No. We can treat the content version as if it is committed
      _content_version->setCommitted();
    }
  }

  // Check the metadata state as well
  Artifact::checkFinalState(path, cache_dir);
}

/// Commit any pending versions and save fingerprints for this artifact
void FileArtifact::applyFinalState(fs::path path, fs::path cache_dir) noexcept {
  // Make sure the content is committed
  _content_version->commit(path);

  // If we don't already have a content fingerprint, take one
  _content_version->fingerprint(path, cache_dir);

  // Call up to fingerprint metadata as well
  Artifact::applyFinalState(path, cache_dir);
}

void FileArtifact::setCommitted() noexcept {
  _content_version->setCommitted();
  Artifact::setCommitted();
}

/// A traced command is about to (possibly) read from this artifact
void FileArtifact::beforeRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void FileArtifact::afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The current content version is an input to command c
  c->currentRun()->addInput(shared_from_this(), _content_version, InputType::Accessed);

  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, _content_version);
}

/// A traced command is about to (possibly) write to this artifact
void FileArtifact::beforeWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The content version is an input to command c
  c->currentRun()->addInput(shared_from_this(), _content_version, InputType::Accessed);

  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, _content_version);
}

/// A traced command just wrote to this artifact
void FileArtifact::afterWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Create a new version
  auto writing = make_shared<FileVersion>();

  // The command wrote to this file
  build.traceUpdateContent(c, ref, writing);
}

/// A traced command is about to truncate this artifact to length 0
void FileArtifact::beforeTruncate(Build& build,
                                  const shared_ptr<Command>& c,
                                  Ref::ID ref) noexcept {
  // Do nothing before a truncate
}

/// A trace command just truncated this artifact to length 0
void FileArtifact::afterTruncate(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The command wrote an empty content version to this artifact
  auto written = make_shared<FileVersion>();
  written->makeEmptyFingerprint();

  build.traceUpdateContent(c, ref, written);
}

// Get this artifact's content without creating dependencies
shared_ptr<Version> FileArtifact::peekContent() noexcept {
  return _content_version;
}

/// Check to see if this artifact's content matches a known version
void FileArtifact::matchContent(Build& build,
                                const shared_ptr<Command>& c,
                                Scenario scenario,
                                shared_ptr<Version> expected) noexcept {
  // The content version is an input to command c
  c->currentRun()->addInput(shared_from_this(), _content_version, InputType::Accessed);

  // Compare the current content version to the expected version
  if (!_content_version->matches(expected)) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, _content_version);
    // Report the mismatch
    c->currentRun()->inputChanged(shared_from_this(), _content_version, expected, scenario);
  }
}

/// Apply a new content version to this artifact
void FileArtifact::updateContent(Build& build,
                                 const shared_ptr<Command>& c,
                                 shared_ptr<Version> writing) noexcept {
  // Add the new version to this artifact
  appendVersion(writing);
  _content_version = writing->as<FileVersion>();

  FAIL_IF(!_content_version) << "Attempted to apply version " << writing << " to file artifact "
                             << this;

  // Report the output to the build
  c->currentRun()->addOutput(shared_from_this(), writing);
}
