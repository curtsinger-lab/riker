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
    c->addContentInput(shared_from_this(), result, _content_writer.lock(), InputType::Accessed);
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
  if (c->running()) {
    _committed_content = sv;
  } else {
    _uncommitted_content = sv;
  }
}

void SymlinkArtifact::commit(shared_ptr<ContentVersion> v) noexcept {
  if (!_uncommitted_content) {
    LOG(artifact) << "Content for " << this << " is already committed";
    return;
  }

  LOG(artifact) << "Committing content to " << this;

  ASSERT(v == _uncommitted_content)
      << "Attempted to commit unknown version " << v << " in " << this;

  // Get a committed path to this artifact, possibly by committing links above it in the path
  auto path = commitPath();
  ASSERT(path.has_value()) << "Symlink has no path";

  // Do the commit
  _uncommitted_content->commit(path.value());
  _committed_content = std::move(_uncommitted_content);
}

// Commit all final versions of this artifact to the filesystem
void SymlinkArtifact::commitAll(optional<fs::path> path) noexcept {
  LOG(artifact) << "Committing content and metadata to " << this;

  // If we weren't given a specific path to commit to, get one by committing links
  if (!path.has_value()) path = commitPath();
  ASSERT(path.has_value()) << "Committing to a symlink with no path";

  // Commit content
  if (_uncommitted_content) {
    _uncommitted_content->commit(path.value());
    _committed_content = std::move(_uncommitted_content);
  }

  // Commit metadata (calls a no-op commitMetadata implementation)
  commitMetadata();
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
