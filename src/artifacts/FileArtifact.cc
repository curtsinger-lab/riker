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

FileArtifact::FileArtifact(shared_ptr<MetadataVersion> mv, shared_ptr<FileVersion> cv) noexcept :
    Artifact(mv) {
  _committed_content = cv;
  appendVersion(cv);
}

bool FileArtifact::canCommit(shared_ptr<ContentVersion> v) const noexcept {
  if (v == _uncommitted_content) {
    return _uncommitted_content->canCommit();
  } else if (v == _committed_content) {
    return true;
  } else {
    FAIL << "Attempted to check committable state for unknown version " << v << " in " << this;
    return false;
  }
}

void FileArtifact::commit(shared_ptr<ContentVersion> v) noexcept {
  if (!_uncommitted_content) {
    LOG(artifact) << "Content for " << this << " is already committed";
    return;
  }

  LOG(artifact) << "Committing content to " << this;

  // Make sure a valid content version was provided
  ASSERT(v == _uncommitted_content)
      << "Attempted to commit unknown version " << v << " in " << this;

  // Get a committed path to this artifact, possibly by committing links above it in the path
  auto path = commitPath();
  ASSERT(path.has_value()) << "Committing to a file with no path";

  // Do the commit
  _uncommitted_content->commit(path.value());
  _committed_content = std::move(_uncommitted_content);
}

/// Do we have saved content and metadata for this artifact?
bool FileArtifact::canCommitAll() const noexcept {
  if (_uncommitted_content) {
    return _uncommitted_content->canCommit();
  } else {
    return true;
  }
}

/// Commit all final versions of this artifact to the filesystem
void FileArtifact::commitAll(optional<fs::path> path) noexcept {
  LOG(artifact) << "Committing content and metadata to " << this;

  // If we weren't given a specific path to commit to, get one by committing links
  if (!path.has_value()) path = commitPath();

  ASSERT(path.has_value()) << "Committing to a file with no path";

  if (_uncommitted_content) {
    _uncommitted_content->commit(path.value());
    _committed_content = std::move(_uncommitted_content);
  }

  commitMetadata(path);
}

/// Compare all final versions of this artifact to the filesystem state
void FileArtifact::checkFinalState(fs::path path) noexcept {
  if (_uncommitted_content) {
    // generate a content fingerprint for the actual file on disk
    auto v = make_shared<FileVersion>();
    auto fingerprint_type = policy::chooseFingerprintType(nullptr, path, v);
    v->fingerprint(path, fingerprint_type);

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_uncommitted_content->matches(v)) {
      // Yes. Report the mismatch
      auto creator = _content_writer.lock();
      if (creator) {
        creator->currentRun()->outputChanged(shared_from_this(), v, _uncommitted_content);
      }

    } else {
      // No. We can treat the content version as if it is committed
      _uncommitted_content->setCommitted();
      _committed_content = std::move(_uncommitted_content);
    }
  }
}

/// Commit any pending versions and save fingerprints for this artifact
void FileArtifact::applyFinalState(fs::path path) noexcept {
  // Make sure the content is committed
  if (_uncommitted_content) {
    _uncommitted_content->commit(path);
    _committed_content = std::move(_uncommitted_content);
  }

  // If we don't already have a content fingerprint, take one
  auto fingerprint_type = policy::chooseFingerprintType(nullptr, path, _committed_content);
  _committed_content->fingerprint(path, fingerprint_type);

  // Cache the contents
  if (policy::isCacheable(nullptr, path, _committed_content)) _committed_content->cache(path);

  // Call up to fingerprint metadata as well
  Artifact::applyFinalState(path);
}

/// A traced command is about to (possibly) read from this artifact
void FileArtifact::beforeRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void FileArtifact::afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, getContent(c));
}

/// A traced command is about to (possibly) write to this artifact
void FileArtifact::beforeWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, getContent(c));
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

// Get this artifact's content version
shared_ptr<ContentVersion> FileArtifact::getContent(const shared_ptr<Command>& c) noexcept {
  auto result = _committed_content;
  if (_uncommitted_content) result = _uncommitted_content;

  ASSERT(result) << "Artifact " << this << " has no content version";

  if (c) {
    c->currentRun()->addContentInput(shared_from_this(), result, _content_writer.lock(),
                                     InputType::Accessed);
  }

  return result;
}

/// Check to see if this artifact's content matches a known version
void FileArtifact::matchContent(const shared_ptr<Command>& c,
                                Scenario scenario,
                                shared_ptr<ContentVersion> expected) noexcept {
  // Get the current content version
  auto observed = getContent(c);

  // Compare the current content version to the expected version
  if (!observed->matches(expected)) {
    // If the observed content version is on disk, try to fingerprint it and try the match again
    if (observed == _committed_content) {
      auto path = getPath(false);
      auto fingerprint_type = policy::chooseFingerprintType(c, path.value(), observed);
      observed->fingerprint(path.value(), fingerprint_type);

      // Try the comparison again. If it succeeds, we can return
      if (observed->matches(expected)) return;
    }

    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, observed);
    // Report the mismatch
    c->currentRun()->inputChanged(shared_from_this(), observed, expected, scenario);
  }
}

/// Apply a new content version to this artifact
void FileArtifact::updateContent(const shared_ptr<Command>& c,
                                 shared_ptr<ContentVersion> writing) noexcept {
  // Add the new version to this artifact
  appendVersion(writing);
  auto fv = writing->as<FileVersion>();

  FAIL_IF(!fv) << "Attempted to apply version " << writing << " to file artifact " << this;

  // Mark the creator of the written version
  writing->createdBy(c);
  _content_writer = c;

  // Is the writer currently running?
  if (c->running()) {
    fv->setCommitted();
    _committed_content = fv;
    _uncommitted_content.reset();
  } else {
    _uncommitted_content = fv;
  }

  // Report the output to the build
  c->currentRun()->addContentOutput(shared_from_this(), writing);
}
