#include "SymlinkArtifact.hh"

#include <cerrno>
#include <filesystem>
#include <memory>

#include "artifacts/DirArtifact.hh"
#include "data/AccessFlags.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/env.hh"
#include "util/log.hh"
#include "util/wrappers.hh"
#include "versions/ContentVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::optional;
using std::shared_ptr;

namespace fs = std::filesystem;

class MetadataVersion;

SymlinkArtifact::SymlinkArtifact(shared_ptr<MetadataVersion> mv,
                                 shared_ptr<SymlinkVersion> sv) noexcept :
    Artifact(mv) {
  _committed_content = sv;
  appendVersion(sv);
}

/// Revert this artifact to its committed state
void SymlinkArtifact::rollback() noexcept {
  _uncommitted_content.reset();
  _content_writer.reset();

  Artifact::rollback();
}

/// A traced command is about to (possibly) read from this artifact
void SymlinkArtifact::beforeRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void SymlinkArtifact::afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, getContent(c));
}

// Get this artifact's current content
shared_ptr<ContentVersion> SymlinkArtifact::getContent(const shared_ptr<Command>& c) noexcept {
  auto result = _committed_content;
  if (_uncommitted_content) result = _uncommitted_content;

  ASSERT(result) << "Artifact " << this << " has no content version";

  if (c) {
    c->addContentInput(shared_from_this(), result, _content_writer.lock());
  }

  return result;
}

/// Check to see if this artifact's content matches a known version
void SymlinkArtifact::matchContent(const shared_ptr<Command>& c,
                                   Scenario scenario,
                                   shared_ptr<ContentVersion> expected) noexcept {
  auto observed = getContent(c);

  // Compare the symlink version to the expected version
  if (!observed->matches(expected)) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, observed);

    // Report the mismatch
    c->inputChanged(shared_from_this(), observed, expected, scenario);
  }
}

// Set the destination of this symlink
void SymlinkArtifact::updateContent(const std::shared_ptr<Command>& c,
                                    std::shared_ptr<ContentVersion> writing) noexcept {
  if (_committed_content || _uncommitted_content) {
    WARN << "Updating destination of symlink " << this << " is not supported by the OS";
  }

  // Set the last writer
  _content_writer = c;

  // Make sure the written version is a SymlinkVersion
  auto sv = writing->as<SymlinkVersion>();

  FAIL_IF(!sv) << "Attempted to apply version " << writing << " to symlink artifact " << this;

  // Set the appropriate content version
  if (c->mustRun()) {
    _committed_content = sv;
  } else {
    _uncommitted_content = sv;
  }
}

// Commit the content of this artifact to the filesystem
void SymlinkArtifact::commitContentTo(fs::path path) noexcept {
  if (!_uncommitted_content) return;

  // Commit the symlink content
  _uncommitted_content->commit(path);

  // Is this commit creating the symlink? (It should be)
  if (!_committed_content) {
    ASSERT(_uncommitted_metadata) << "Committing initial content to " << this
                                  << " does not have metadata to commit";

    // Treat the metadata as committed
    _committed_metadata = std::move(_uncommitted_metadata);
  }

  // Remember the committed content now
  _committed_content = std::move(_uncommitted_content);
}

/// Commit a link to this artifact at the given path
void SymlinkArtifact::commitLink(shared_ptr<DirEntry> entry) noexcept {
  // Check for a matching committed link. If we find one, return.
  auto iter = _committed_links.find(entry);
  if (iter != _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = entry->getDir()->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto new_path = dir_path / entry->getName();

  // Three cases to handle for symlinks:
  // 1. The symlink has a temporary path. Move it into place.
  // 2. The symlink has another hard link already. This case isn't handled at the moment
  // 3. The symlink has no committed links. Commit content to create one
  if (auto temp_path = takeTemporaryPath(); temp_path.has_value()) {
    // This artifact has a temporary path. We can move it to its new committed location
    LOG(artifact) << "Moving " << this << " from temporary location to " << new_path;

    // Yes. Move the artifact into place
    int rc = ::rename(temp_path.value().c_str(), new_path.c_str());
    FAIL_IF(rc != 0) << "Failed to move " << this << " from a temporary location: " << ERR;

  } else if (auto committed_path = getCommittedPath(); committed_path.has_value()) {
    // The symlink has an existing committed path. Bail on this for now.
    FAIL << "Artifact " << this << " already has a committed path, and cannot be hard linked.";

  } else {
    ASSERT(hasUncommittedContent()) << "Artifact has no committed path, but content is committed";

    // This artifact has no paths. Create one by committing its content.
    commitContentTo(new_path);
  }

  // Record the committed link
  _committed_links.emplace_hint(iter, entry);
  return;
}

/// Commit an unlink of this artifact at the given path
void SymlinkArtifact::commitUnlink(shared_ptr<DirEntry> entry) noexcept {
  // Check for a matching committed link. If we don't find one, return immediately.
  auto iter = _committed_links.find(entry);
  if (iter == _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = entry->getDir()->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto unlink_path = dir_path / entry->getName();

  // Committing an unlink of a symlink has two cases:
  // 1. There are uncommitted links, but no other committed links. Move to a temporary path.
  // 2. Otherwise just unlink
  if (_committed_links.size() == 1 && _modeled_links.size() > 0) {
    LOG(artifact) << "Unlinking " << this << " at " << unlink_path << ": move to temporary path";

    // Get a temporary path for this file
    auto temp_path = assignTemporaryPath();

    // Move the file
    int rc = ::rename(unlink_path.c_str(), temp_path.c_str());
    FAIL_IF(rc != 0) << "Failed to move " << this << " to a temporary location: " << ERR;

  } else {
    LOG(artifact) << "Unlinking " << this << " at " << unlink_path << ": remove";

    // It's safe to just unlink the file.
    int rc = ::unlink(unlink_path.c_str());
    FAIL_IF(rc != 0) << "Failed to unlink " << this << " from " << unlink_path << ": " << ERR;
  }

  // Remove the committed link and return
  _committed_links.erase(iter);
  return;
}

// Compare all final versions of this artifact to the filesystem state
void SymlinkArtifact::checkFinalState(fs::path path) noexcept {
  if (_uncommitted_content) {
    // TODO: Compare to on-disk symlink state here
  }
}

// Commit any pending versions and save fingerprints for this artifact
void SymlinkArtifact::applyFinalState(fs::path path) noexcept {
  // Symlinks are always saved, so no need to fingerprint

  // Make sure this symlink is committed
  if (_uncommitted_content) {
    _uncommitted_content->commit(path);
    _committed_content = std::move(_uncommitted_content);
  }

  // TODO: commit ownership but not permissions from metadata
}

Ref SymlinkArtifact::resolve(const shared_ptr<Command>& c,
                             shared_ptr<Artifact> prev,
                             fs::path::iterator current,
                             fs::path::iterator end,
                             AccessFlags flags,
                             size_t symlink_limit) noexcept {
  if (symlink_limit == 0) return ELOOP;

  // If this is the end of the path and the nofollow flag is set, return this symlink
  if (current == end && flags.nofollow) {
    // Did the access expect to get a symlink?
    if (flags.type == AccessType::Dir) {
      return ENOTDIR;
    } else if (flags.type == AccessType::File) {
      return ELOOP;
    } else {
      return Ref(flags, shared_from_this());
    }
  }

  // Get the symlink destination
  auto dest = getContent(c)->as<SymlinkVersion>()->getDestination();

  // Append remaining path entries to the destination
  while (current != end) {
    dest /= *current++;
  }

  // Is the destination relative or absolute?
  if (dest.is_relative()) {
    // Resolve relative to the previous artifact, which must be the dir that holds this symlink
    return prev->resolve(c, dest, flags, symlink_limit - 1);

  } else {
    // Strip the leading slash from the path
    dest = dest.relative_path();

    // Resolve relative to root. First strip the leading slash off the path
    return env::getRootDir()->resolve(c, dest, flags, symlink_limit - 1);
  }
}
