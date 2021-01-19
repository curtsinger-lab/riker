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
#include "versions/ContentVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::optional;
using std::shared_ptr;

namespace fs = std::filesystem;

class MetadataVersion;

SymlinkArtifact::SymlinkArtifact(std::shared_ptr<SymlinkVersion> sv) noexcept : Artifact() {
  appendVersion(sv);
  _symlink_version = sv;
}

SymlinkArtifact::SymlinkArtifact(shared_ptr<MetadataVersion> mv,
                                 shared_ptr<SymlinkVersion> sv) noexcept :
    Artifact(mv) {
  appendVersion(sv);
  _symlink_version = sv;
}

/// A traced command is about to (possibly) read from this artifact
void SymlinkArtifact::beforeRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void SymlinkArtifact::afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The command now depends on the content of this file
  build.traceMatchContent(c, ref, _symlink_version);
}

// Get this artifact's content without creating dependencies
shared_ptr<ContentVersion> SymlinkArtifact::peekContent() noexcept {
  return _symlink_version;
}

/// Check to see if this artifact's content matches a known version
void SymlinkArtifact::matchContent(const shared_ptr<Command>& c,
                                   Scenario scenario,
                                   shared_ptr<ContentVersion> expected) noexcept {
  // The symlink version is an input to command c
  c->currentRun()->addContentInput(shared_from_this(), _symlink_version, InputType::Accessed);

  // Compare the symlink version to the expected version
  if (!_symlink_version->matches(expected)) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, _symlink_version);

    // Report the mismatch
    c->currentRun()->inputChanged(shared_from_this(), _symlink_version, expected, scenario);
  }
}

bool SymlinkArtifact::canCommit(shared_ptr<ContentVersion> v) const noexcept {
  ASSERT(v == _symlink_version) << "Attempted to check committable state for unknown version " << v
                                << " in " << this;
  return _symlink_version->canCommit();
}

void SymlinkArtifact::commit(shared_ptr<ContentVersion> v) noexcept {
  LOG(artifact) << "Committing content to " << this;

  // Get a committed path to this artifact, possibly by committing links above it in the path
  auto path = commitPath();
  ASSERT(path.has_value()) << "Symlink has no path";

  ASSERT(v == _symlink_version) << "Attempted to commit unknown version " << v << " in " << this;
  _symlink_version->commit(path.value());
}

bool SymlinkArtifact::canCommitAll() const noexcept {
  // Symlink versions are always committable
  return true;
}

// Commit all final versions of this artifact to the filesystem
void SymlinkArtifact::commitAll(optional<fs::path> path) noexcept {
  LOG(artifact) << "Committing content and metadata to " << this;

  // If we weren't given a specific path to commit to, get one by committing links
  if (!path.has_value()) path = commitPath();
  ASSERT(path.has_value()) << "Committing to a symlink with no path";

  _symlink_version->commit(path.value());

  // Don't commit symlink metadata for now. We can eventually commit ownership, but not
  // permissions.
  commitMetadata();
}

// Compare all final versions of this artifact to the filesystem stateq
void SymlinkArtifact::checkFinalState(fs::path path) noexcept {
  if (!_symlink_version->isCommitted()) {
    // TODO: Compare to on-disk symlink state here
  }
}

// Commit any pending versions and save fingerprints for this artifact
void SymlinkArtifact::applyFinalState(fs::path path) noexcept {
  // Symlinks are always saved, so no need to fingerprint

  // Make sure this symlink is committed
  _symlink_version->commit(path);

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

  // Otherwise we follow the symlink. That creates a path resolution dependency on our version
  c->currentRun()->addContentInput(shared_from_this(), _symlink_version, InputType::PathResolution);

  // Get the symlink destination
  auto dest = _symlink_version->getDestination();

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
