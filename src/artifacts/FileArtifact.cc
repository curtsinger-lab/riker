#include "FileArtifact.hh"

#include <filesystem>
#include <memory>
#include <optional>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
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

// Commit the content of this artifact to the filesystem
void FileArtifact::commitContentTo(fs::path path) noexcept {
  if (!_uncommitted_content) return;

  _uncommitted_content->commit(path);
  _committed_content = std::move(_uncommitted_content);
}

/// Commit a link to this artifact at the given path
void FileArtifact::commitLink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  // Check for a matching committed link. If we find one, return.
  auto iter = _committed_links.find(Link{dir, entry});
  if (iter != _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = dir->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto new_path = dir_path / entry;

  // Committing a new path to this artifact has three cases:
  // 1. The file has a temporary path. Move it into place
  // 2. The file has an existing committed path. Create a hard link
  // 3. The file has no committed paths. Commit its content to create the file
  if (auto temp_path = takeTemporaryPath(); temp_path.has_value()) {
    // This artifact has a temporary path. We can move it to its new committed location
    LOG(artifact) << "Moving " << this << " from temporary location to " << dir_path / entry;

    // Yes. Move the artifact into place
    int rc = ::rename(temp_path.value().c_str(), new_path.c_str());
    FAIL_IF(rc != 0) << "Failed to move " << this << " from a temporary location: " << ERR;

  } else if (auto committed_path = getCommittedPath(); committed_path.has_value()) {
    // This artifact has another committed path. We can create a hard link from that path
    int rc = ::link(committed_path.value().c_str(), new_path.c_str());
    FAIL_IF(rc != 0) << "Failed to hard link " << this << " to " << new_path << ": " << ERR;

  } else {
    ASSERT(hasUncommittedContent()) << "Artifact has no committed path, but content is committed";

    // This artifact has no paths. Create one by committing its content.
    commitContentTo(new_path);
  }

  // Record the committed link and return
  _committed_links.emplace_hint(iter, Link{dir, entry});
  return;
}

/// Commit an unlink of this artifact at the given path
void FileArtifact::commitUnlink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  // Check for a matching committed link. If we don't find one, return immediately.
  auto iter = _committed_links.find(Link{dir, entry});
  if (iter == _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = dir->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto unlink_path = dir_path / entry;

  // Committing an unlink of a file has two cases:
  // 1. There are uncommitted links, but no other committed links. Move to a temporary path.
  // 2. Otherwise just unlink
  if (_committed_links.size() == 1 && _modeled_links.size() > 0) {
    // Get a temporary path for this file
    auto temp_path = assignTemporaryPath();

    // Move the file
    int rc = ::rename(unlink_path.c_str(), temp_path.c_str());
    FAIL_IF(rc != 0) << "Failed to move " << this << " to a temporary location: " << ERR;

  } else {
    // It's safe to just unlink the file.
    int rc = ::unlink(unlink_path.c_str());
    FAIL_IF(rc != 0) << "Failed to unlink " << this << " from " << unlink_path << ": " << ERR;
  }

  // Remove the committed link and return
  _committed_links.erase(iter);
  return;
}

/// Compare all final versions of this artifact to the filesystem state
void FileArtifact::checkFinalState(fs::path path) noexcept {
  // Get the command that wrote this file. If there was no writer, no need to check
  auto creator = _content_writer.lock();
  if (!creator) return;

  // Is there an uncommitted update to this file?
  if (_uncommitted_content) {
    // Does the uncommitted version match what's on the filesystem?
    bool matches = _uncommitted_content->matches(_committed_content);

    // If there was no match, try again with a fingerprint
    if (!matches) {
      auto v = _committed_content;
      if (!v) v = make_shared<FileVersion>();
      auto fingerprint_type = policy::chooseFingerprintType(nullptr, nullptr, path);
      v->fingerprint(path, fingerprint_type);

      matches = _uncommitted_content->matches(v);
    }

    // Were we able to find a match?
    if (matches) {
      // Yes. No work to do here.
      // TODO: We could possible treat this artifact as already-committed, but we have to make sure
      // its pending links are committed correctly too.

    } else {
      // No. The creating command has to rerun.
      creator->outputChanged(shared_from_this(), _committed_content, _uncommitted_content);
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
  auto fingerprint_type = policy::chooseFingerprintType(nullptr, _content_writer.lock(), path);
  _committed_content->fingerprint(path, fingerprint_type);

  // Cache the contents
  if (policy::isCacheable(nullptr, _content_writer.lock(), path)) {
    _committed_content->cache(path);
  }

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
  fingerprintAndCache(c);
  build.traceMatchContent(c, ref, getContent(c));
}

/// A traced command is about to (possibly) write to this artifact
void FileArtifact::beforeWrite(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The command now depends on the content of this file
  fingerprintAndCache(c);
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
    c->addContentInput(shared_from_this(), result, _content_writer.lock(), InputType::Accessed);
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
      auto path = getCommittedPath();
      auto fingerprint_type =
          policy::chooseFingerprintType(c, _content_writer.lock(), path.value());
      observed->fingerprint(path.value(), fingerprint_type);

      // Try the comparison again. If it succeeds, we can return
      if (observed->matches(expected)) return;
    }

    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, observed);
    // Report the mismatch
    c->inputChanged(shared_from_this(), observed, expected, scenario);
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
  _content_writer = c;

  // Is the writer currently running?
  if (c->running() || c->alreadyRun()) {
    _committed_content = fv;
    _uncommitted_content.reset();
  } else {
    _uncommitted_content = fv;
  }

  // Report the output to the build
  c->addContentOutput(shared_from_this(), writing);
}

void FileArtifact::fingerprintAndCache(const shared_ptr<Command>& reader) noexcept {
  // If this artifact does not have a committed version, it can't be cached or fingerprinted
  if (!_committed_content) return;

  // Get a path to this artifact
  auto path = getCommittedPath();

  // If the artifact has a committed path, we may fingerprint or cache it
  if (path.has_value()) {
    auto fingerprint_type =
        policy::chooseFingerprintType(reader, _content_writer.lock(), path.value());
    _committed_content->fingerprint(path.value(), fingerprint_type);

    // cache?
    if (policy::isCacheable(reader, _content_writer.lock(), path.value())) {
      _committed_content->cache(path.value());
    }
  }
}
