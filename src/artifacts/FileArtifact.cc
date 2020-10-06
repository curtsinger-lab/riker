#include "FileArtifact.hh"

#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "interfaces/BuildObserver.hh"
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
    _metadata_version->commit(path.value());
  } else {
    FAIL << "Attempted to commit unknown version " << v << " in " << this;
  }
}

// Do we have saved content and metadata for this artifact?
bool FileArtifact::canCommitAll() const noexcept {
  // Can the metadata version be committed?
  if (!_metadata_version->canCommit()) return false;

  return _content_version->canCommit();
}

// Commit all final versions of this artifact to the filesystem
void FileArtifact::commitAll() noexcept {
  // we may have already committed this artifact
  if (_content_version->isCommitted() && _metadata_version->isCommitted()) return;

  auto path = getPath();
  ASSERT(path.has_value()) << "File has no path: " << this;

  _content_version->commit(path.value(), _metadata_version->getMode());
  _metadata_version->commit(path.value());
}

// Command c requires that this artifact exists in its current state. Create dependency edges.
void FileArtifact::mustExist(Build& build, shared_ptr<Command> c) noexcept {
  build.observeInput(c, shared_from_this(), _metadata_version, InputType::Exists);
  build.observeInput(c, shared_from_this(), _content_version, InputType::Exists);
}

// Compare all final versions of this artifact to the filesystem state
void FileArtifact::checkFinalState(Build& build, fs::path path) noexcept {
  if (!_content_version->isCommitted()) {
    auto v = make_shared<FileVersion>();
    v->fingerprint(build, path);

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_content_version->matches(v)) {
      // Yes. Report the mismatch
      build.observeFinalMismatch(shared_from_this(), _content_version, v);
    } else {
      // No. We can treat the content version as if it is committed
      _content_version->setCommitted();
    }
  }

  // Check the metadata state as well
  Artifact::checkFinalState(build, path);
}

// Commit any pending versions and save fingerprints for this artifact
void FileArtifact::applyFinalState(Build& build, fs::path path) noexcept {
  // If we don't already have a content fingerprint, take one
  if (!_content_version->hasFingerprint()) {
    ASSERT(_content_version->isCommitted()) << "Cannot fingerprint an uncommitted version";
    _content_version->fingerprint(build, path);
  }

  // Make sure the content is committed
  _content_version->commit(path);

  // Call up to fingerprint metadata as well
  Artifact::applyFinalState(build, path);
}

void FileArtifact::setCommitted() noexcept {
  _content_version->setCommitted();
  Artifact::setCommitted();
}

/// A traced command is about to (possibly) read from this artifact
void FileArtifact::beforeRead(Build& build,
                              shared_ptr<Command> c,
                              shared_ptr<RefResult> ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void FileArtifact::afterRead(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<RefResult> ref) noexcept {
  // The current content version is an input to command c
  build.observeInput(c, shared_from_this(), _content_version, InputType::Accessed);

  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, _content_version);
}

/// A traced command is about to (possibly) write to this artifact
void FileArtifact::beforeWrite(Build& build,
                               shared_ptr<Command> c,
                               shared_ptr<RefResult> ref) noexcept {
  // The content version is an input to command c
  build.observeInput(c, shared_from_this(), _content_version, InputType::Accessed);

  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, _content_version);
}

/// A trace command just wrote to this artifact
void FileArtifact::afterWrite(Build& build,
                              shared_ptr<Command> c,
                              shared_ptr<RefResult> ref) noexcept {
  // The command wrote to this file
  build.traceUpdateContent(c, ref);
}

/// Check to see if this artifact's content matches a known version
void FileArtifact::matchContent(Build& build,
                                shared_ptr<Command> c,
                                shared_ptr<Version> expected) noexcept {
  // The content version is an input to command c
  build.observeInput(c, shared_from_this(), _content_version, InputType::Accessed);

  // Compare the current content version to the expected version
  if (!_content_version->matches(expected)) {
    // Report the mismatch
    build.observeMismatch(c, shared_from_this(), _content_version, expected);
  }
}

/// Create a new version to hold contents for this artifact
shared_ptr<Version> FileArtifact::createContentVersion() noexcept {
  return make_shared<FileVersion>();
}

/// Apply a new content version to this artifact
void FileArtifact::updateContent(Build& build,
                                 shared_ptr<Command> c,
                                 shared_ptr<FileVersion> writing) noexcept {
  // Add the new version to this artifact
  appendVersion(writing);
  _content_version = writing;

  // Report the output to the build
  build.observeOutput(c, shared_from_this(), _content_version);
}
