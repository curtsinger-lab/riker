#include "FileArtifact.hh"

#include <filesystem>
#include <memory>
#include <optional>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/policy.hh"
#include "util/log.hh"
#include "util/options.hh"
#include "versions/ContentVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"

using std::make_shared;
using std::optional;
using std::shared_ptr;

namespace fs = std::filesystem;

class MetadataVersion;

FileArtifact::FileArtifact(MetadataVersion mv, shared_ptr<FileVersion> cv) noexcept : Artifact(mv) {
  _content.update(cv);
  appendVersion(cv);
}

/// Revert this artifact to its committed state
void FileArtifact::rollback() noexcept {
  _content.rollback();
  Artifact::rollback();
}

// Commit the content of this artifact to the filesystem
void FileArtifact::commitContentTo(fs::path path) noexcept {
  // If content is already committed, do nothing
  if (_content.isCommitted()) return;

  // Does this artifact already have committed content?
  if (_content.hasCommittedState()) {
    // Yes. Just commit the content version
    auto [version, writer] = _content.getLatest();
    ASSERT(version->canCommit()) << "Cannot commit content " << _content;

    // Commit the uncommitted content only
    version->commit(path);

  } else {
    // No committed content yet. Commit metadata along with the content
    ASSERT(_uncommitted_metadata)
        << "File with no committed content does not have uncommitted metadata";

    // Commit the content with initial metadata
    auto [version, writer] = _content.getLatest();
    version->commit(path, _uncommitted_metadata->getMode());
    Artifact::setMetadataCommitted();
  }

  // The artifact content is now committed
  _content.setCommitted();
}

/// Commit a link to this artifact at the given path
void FileArtifact::commitLink(shared_ptr<DirEntry> entry) noexcept {
  // Check for a matching committed link. If we find one, return.
  auto iter = _committed_links.find(entry);
  if (iter != _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = entry->getDir()->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto new_path = dir_path / entry->getName();

  // Committing a new path to this artifact has three cases:
  // 1. The file has a temporary path. Move it into place
  // 2. The file has an existing committed path. Create a hard link
  // 3. The file has no committed paths. Commit its content to create the file
  if (auto temp_path = takeTemporaryPath(); temp_path.has_value()) {
    // This artifact has a temporary path. We can move it to its new committed location
    LOG(artifact) << "Moving " << this << " from temporary location to " << new_path;

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
  _committed_links.emplace_hint(iter, entry);
  return;
}

/// Commit an unlink of this artifact at the given path
void FileArtifact::commitUnlink(shared_ptr<DirEntry> entry) noexcept {
  // Check for a matching committed link. If we don't find one, return immediately.
  auto iter = _committed_links.find(entry);
  if (iter == _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = entry->getDir()->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto unlink_path = dir_path / entry->getName();

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
  auto [version, weak_creator] = _content.getLatest();
  auto creator = weak_creator.lock();
  if (!creator) return;

  // Is there an uncommitted update to this file?
  if (_content.isUncommitted()) {
    // Get the committed state
    auto [committed_version, committed_creator] = _content.getCommitted();

    // Does the uncommitted version match what's on the filesystem?
    bool matches = version->matches(committed_version);

    // If there was no match, try again with a fingerprint
    // TODO: Re-enable this once we check for a match between the committed and uncommitted state
    // at commit time
    /*if (!matches) {
      auto v = _committed_content;
      if (!v) v = make_shared<FileVersion>();
      auto fingerprint_type = policy::chooseFingerprintType(nullptr, nullptr, path);
      v->fingerprint(path, fingerprint_type);

      matches = _uncommitted_content->matches(v);
      _committed_content = v;
    }*/

    // Were we able to find a match?
    if (matches) {
      // Yes. No work to do here.
      // TODO: We could possible treat this artifact as already-committed, but we have to make
      // sure its pending links are committed correctly too.

    } else {
      // No. The creating command has to rerun.
      creator->outputChanged(shared_from_this(), committed_version, version);
    }
  }

  fingerprintAndCache(nullptr);
}

/// Commit any pending versions and save fingerprints for this artifact
void FileArtifact::applyFinalState(fs::path path) noexcept {
  // Get the content version and creator
  auto [version, weak_creator] = _content.getLatest();
  auto creator = weak_creator.lock();

  // Make sure the content is committed
  if (_content.isUncommitted()) {
    auto [committed_version, committed_creator] = _content.getCommitted();

    // Does the uncommitted version match the committed version?
    if (version->matches(committed_version)) {
      // Yes.
      // TODO: Treat the uncommitted version as committed?

    } else {
      // No. Commit now
      version->commit(path);
      _content.setCommitted();
    }
  }

  // If we don't already have a content fingerprint, take one
  auto fingerprint_type = policy::chooseFingerprintType(nullptr, creator, path);
  version->fingerprint(path, fingerprint_type);

  // Cache the contents
  if (policy::isCacheable(nullptr, creator, path)) {
    version->cache(path);
  }

  // Call up to fingerprint metadata as well
  Artifact::applyFinalState(path);
}

/// Fingerprint and cache the committed state of this artifact
void FileArtifact::cacheAll(fs::path path) const noexcept {
  fingerprintAndCache(nullptr);
}

/// A traced command is about to stat this artifact
void FileArtifact::beforeStat(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Create a dependency on the current content so its size is committed appropriately.
  getContent(c);

  // TODO: This should really be captured in the trace by including size as part of the metadata.
  // Committing metadata size changes is tricky because we would need to commit the content to
  // change the size field. Or, we could modify the result from stat to report the uncommitted
  // size during tracing. If we go that route, writes need to update both content and metadata.
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
  // Get the latest version and writer
  auto [version, weak_writer] = _content.getLatest();
  auto writer = weak_writer.lock();

  // If there is a reading command, record the input
  if (c) c->addContentInput(shared_from_this(), version, writer);

  return version;
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
    if (_content.isCommitted()) {
      // Get the content version and writer
      auto [committed_content, weak_committed_writer] = _content.getLatest();
      auto committed_writer = weak_committed_writer.lock();

      // Get a path
      auto path = getCommittedPath();

      // Try the match
      auto fingerprint_type = policy::chooseFingerprintType(c, committed_writer, path.value());
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

  // Update the content version(s)
  _content.update(c, fv);

  // Report the output to the build
  c->addContentOutput(shared_from_this(), writing);
}

void FileArtifact::fingerprintAndCache(const shared_ptr<Command>& reader) const noexcept {
  // If this artifact is not committed in its latest state, we can't fingerprint or cache it
  if (!_content.isCommitted()) return;

  auto [version, weak_writer] = _content.getLatest();
  auto writer = weak_writer.lock();

  // If the reader is also the last writer, there's no need to fingerprint or cache
  if (reader == writer) return;

  // Get a path to this artifact
  auto path = getCommittedPath();

  // If the artifact has a committed path, we may fingerprint or cache it
  if (path.has_value()) {
    auto fingerprint_type = policy::chooseFingerprintType(reader, writer, path.value());
    version->fingerprint(path.value(), fingerprint_type);

    // cache?
    if (!version->canCommit() && policy::isCacheable(reader, writer, path.value())) {
      version->cache(path.value());
    }
  }
}
