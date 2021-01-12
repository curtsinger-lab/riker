#include "FileArtifact.hh"

#include <filesystem>
#include <memory>
#include <optional>

#include "artifacts/Artifact.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/policy.hh"
#include "ui/options.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/FileVersion.hh"

using std::make_shared;
using std::optional;
using std::shared_ptr;

namespace fs = std::filesystem;

class MetadataVersion;

FileArtifact::FileArtifact(shared_ptr<FileVersion> cv) noexcept : Artifact() {
  appendVersion(cv);
  _content_version = cv;
}

FileArtifact::FileArtifact(shared_ptr<MetadataVersion> mv, shared_ptr<FileVersion> cv) noexcept :
    Artifact(mv) {
  appendVersion(cv);
  _content_version = cv;
}

bool FileArtifact::canCommit(shared_ptr<ContentVersion> v) const noexcept {
  ASSERT(v == _content_version) << "Attempted to check committable state for unknown version " << v
                                << " in " << this;
  return _content_version->canCommit();
}

void FileArtifact::commit(shared_ptr<ContentVersion> v) noexcept {
  LOG(artifact) << "Committing content to " << this;

  // Get a committed path to this artifact, possibly by committing links above it in the path
  auto path = commitPath();
  ASSERT(path.has_value()) << "Committing to a file with no path";

  ASSERT(v == _content_version) << "Attempted to commit unknown version " << v << " in " << this;
  _content_version->commit(path.value());
}

/// Do we have saved content and metadata for this artifact?
bool FileArtifact::canCommitAll() const noexcept {
  return _content_version->canCommit();
}

/// Commit all final versions of this artifact to the filesystem
void FileArtifact::commitAll(optional<fs::path> path) noexcept {
  LOG(artifact) << "Committing content and metadata to " << this;

  // If we weren't given a specific path to commit to, get one by committing links
  if (!path.has_value()) path = commitPath();

  ASSERT(path.has_value()) << "Committing to a file with no path";

  _content_version->commit(path.value());
  Artifact::commitMetadata(path);
}

/// Compare all final versions of this artifact to the filesystem state
void FileArtifact::checkFinalState(fs::path path) noexcept {
  if (!_content_version->isCommitted()) {
    // generate a content fingerprint for the actual file on disk
    auto v = make_shared<FileVersion>();
    auto fingerprint_type = policy::chooseFingerprintType(nullptr, path, v);
    v->fingerprint(path, fingerprint_type);

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_content_version->matches(v)) {
      // Yes. Report the mismatch
      auto creator = _content_version->getCreator();
      if (creator) creator->currentRun()->outputChanged(shared_from_this(), v, _content_version);

    } else {
      // No. We can treat the content version as if it is committed
      _content_version->setCommitted();
    }
  }
}

/// Commit any pending versions and save fingerprints for this artifact
void FileArtifact::applyFinalState(fs::path path) noexcept {
  // Make sure the content is committed
  _content_version->commit(path);

  // If we don't already have a content fingerprint, take one
  auto fingerprint_type = policy::chooseFingerprintType(nullptr, path, _content_version);
  _content_version->fingerprint(path, fingerprint_type);

  // Cache the contents
  if (policy::isCacheable(nullptr, path, _content_version)) _content_version->cache(path);

  // Call up to fingerprint metadata as well
  Artifact::applyFinalState(path);
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
  c->currentRun()->addContentInput(shared_from_this(), _content_version, InputType::Accessed);

  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, _content_version);
}

/// A traced command is about to (possibly) write to this artifact
void FileArtifact::beforeWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The content version is an input to command c
  c->currentRun()->addContentInput(shared_from_this(), _content_version, InputType::Accessed);

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
shared_ptr<ContentVersion> FileArtifact::peekContent() noexcept {
  return _content_version;
}

/// Check to see if this artifact's content matches a known version
void FileArtifact::matchContent(const shared_ptr<Command>& c,
                                Scenario scenario,
                                shared_ptr<ContentVersion> expected) noexcept {
  // The content version is an input to command c
  c->currentRun()->addContentInput(shared_from_this(), _content_version, InputType::Accessed);

  // This is a short-term workaround for lazy builds. If the content version is committed, make sure
  // it has a fingerprint.
  // TODO: Remove this once artifacts track committed and uncommitted state
  if (!options::eager_builds && _content_version->isCommitted()) {
    auto path = getPath(false);
    auto fingerprint_type = policy::chooseFingerprintType(c, path.value(), _content_version);
    _content_version->fingerprint(path.value(), fingerprint_type);
  }

  // Compare the current content version to the expected version
  if (!_content_version->matches(expected)) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, _content_version);
    // Report the mismatch
    c->currentRun()->inputChanged(shared_from_this(), _content_version, expected, scenario);
  }
}

/// Apply a new content version to this artifact
void FileArtifact::updateContent(const shared_ptr<Command>& c,
                                 shared_ptr<ContentVersion> writing) noexcept {
  // Add the new version to this artifact
  appendVersion(writing);
  _content_version = writing->as<FileVersion>();

  FAIL_IF(!_content_version) << "Attempted to apply version " << writing << " to file artifact "
                             << this;

  // Report the output to the build
  c->currentRun()->addContentOutput(shared_from_this(), writing);
}
