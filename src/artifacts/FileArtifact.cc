#include "FileArtifact.hh"

#include <memory>
#include <string>

#include "artifacts/Artifact.hh"
#include "build/Build.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::shared_ptr;
using std::string;

FileArtifact::FileArtifact(Env& env,
                           shared_ptr<MetadataVersion> mv,
                           shared_ptr<FileVersion> cv) noexcept :
    Artifact(env, mv) {
  appendVersion(cv);
  _content_version = cv;
}

bool FileArtifact::canCommit(shared_ptr<Version> v) const noexcept {
  if (v == _content_version) {
    return _content_version->canCommit();
  } else {
    return Artifact::canCommit(v);
  }
}

void FileArtifact::commit(shared_ptr<Version> v) noexcept {
  if (v == _content_version) {
    auto path = getCommittedPath();
    ASSERT(path.has_value()) << "File has no path";
    _content_version->commit(path.value());
  } else {
    Artifact::commit(v);
  }
}

// Do we have saved content and metadata for this artifact?
bool FileArtifact::canCommitAll() const noexcept {
  return _content_version->canCommit() && Artifact::canCommitAll();
}

// Commit all final versions of this artifact to the filesystem
void FileArtifact::commitAll() noexcept {
  auto path = getCommittedPath();
  ASSERT(path.has_value()) << "File has no path: " << this;

  _content_version->commit(path.value());

  // Delegate metadata commits to the artifact
  Artifact::commitAll();
}

// Command c requires that this artifact exists in its current state. Create dependency edges.
void FileArtifact::mustExist(shared_ptr<Command> c) noexcept {
  _env.getBuild().observeInput(c, shared_from_this(), _metadata_version, InputType::Exists);
  _env.getBuild().observeInput(c, shared_from_this(), _content_version, InputType::Exists);
}

// Compare all final versions of this artifact to the filesystem state
void FileArtifact::checkFinalState(fs::path path) noexcept {
  if (!_content_version->isCommitted()) {
    auto v = make_shared<FileVersion>();
    v->fingerprint(path);

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_content_version->matches(v)) {
      // Yes. Report the mismatch
      _env.getBuild().observeFinalMismatch(shared_from_this(), _content_version, v);
    } else {
      // No. We can treat the content version as if it is committed
      _content_version->setCommitted();
    }
  }

  // Check the metadata state as well
  Artifact::checkFinalState(path);
}

// Commit any pending versions and save fingerprints for this artifact
void FileArtifact::applyFinalState(fs::path path) noexcept {
  // If we don't already have a content fingerprint, take one
  if (!_content_version->hasFingerprint()) {
    ASSERT(_content_version->isCommitted()) << "Cannot fingerprint an uncommitted version";
    _content_version->fingerprint(path);
  }

  // Make sure the content is committed
  _content_version->commit(path);

  // Call up to fingerprint metadata as well
  Artifact::applyFinalState(path);
}

/// Get the current content version for this artifact
shared_ptr<FileVersion> FileArtifact::getContent(shared_ptr<Command> c, InputType t) noexcept {
  // Notify the build of the input
  _env.getBuild().observeInput(c, shared_from_this(), _content_version, t);

  // Return the metadata version
  return _content_version;
}

/// Check to see if this artifact's content matches a known version
void FileArtifact::match(shared_ptr<Command> c, shared_ptr<FileVersion> expected) noexcept {
  // Get the current metadata
  auto observed = getContent(c, InputType::Accessed);

  // Compare versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    _env.getBuild().observeMismatch(c, shared_from_this(), observed, expected);
  }
}

/// Apply a new content version to this artifact
void FileArtifact::apply(shared_ptr<Command> c, shared_ptr<FileVersion> writing) noexcept {
  // Add the new version to this artifact
  appendVersion(writing);
  _content_version = writing;

  // Report the output to the build
  _env.getBuild().observeOutput(c, shared_from_this(), _content_version);
}
