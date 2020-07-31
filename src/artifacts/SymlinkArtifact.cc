#include "SymlinkArtifact.hh"

#include <memory>

#include "build/Build.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::shared_ptr;

SymlinkArtifact::SymlinkArtifact(shared_ptr<Env> env,
                                 shared_ptr<MetadataVersion> mv,
                                 shared_ptr<SymlinkVersion> sv) noexcept :
    Artifact(env, mv) {
  appendVersion(sv);
  _symlink_version = sv;
}

// Get the current symlink version of this artifact
shared_ptr<Version> SymlinkArtifact::getContent(Build& build,
                                                shared_ptr<Command> c,
                                                InputType t) noexcept {
  // Notify the build of the input
  build.observeInput(c, shared_from_this(), _symlink_version, t);

  // Return the metadata version
  return _symlink_version;
}

bool SymlinkArtifact::canCommit(shared_ptr<Version> v) const noexcept {
  if (v == _symlink_version) {
    return true;
  } else if (v == _metadata_version) {
    return _metadata_version->canCommit();
  } else {
    FAIL << "Attempted to check committable state for unknown version " << v << " in " << this;
    return false;
  }
}

void SymlinkArtifact::commit(shared_ptr<Version> v) noexcept {
  auto path = getPath();
  ASSERT(path.has_value()) << "Symlink has no path";

  if (v == _symlink_version) {
    _symlink_version->commit(path.value());
  } else if (v == _metadata_version) {
    _metadata_version->commit(path.value(), false);
  } else {
    FAIL << "Attempted to commit unknown version " << v << " in " << this;
  }
}

bool SymlinkArtifact::canCommitAll() const noexcept {
  // Symlink versions are always committable, so just check the metadata version
  return _metadata_version->canCommit();
}

// Commit all final versions of this artifact to the filesystem
void SymlinkArtifact::commitAll() noexcept {
  auto path = getPath();
  ASSERT(path.has_value()) << "Symlink has no path";

  _symlink_version->commit(path.value());
  _metadata_version->commit(path.value(), false);
}

// Command c requires that this artifact exists in its current state. Create dependency edges.
void SymlinkArtifact::mustExist(Build& build, shared_ptr<Command> c) noexcept {
  build.observeInput(c, shared_from_this(), _metadata_version, InputType::Exists);
  build.observeInput(c, shared_from_this(), _symlink_version, InputType::Exists);
}

// Compare all final versions of this artifact to the filesystem state
void SymlinkArtifact::checkFinalState(Build& build, fs::path path) noexcept {
  if (!_symlink_version->isCommitted()) {
    // TODO: Compare to on-disk symlink state here
  }

  // Check the metadata state as well
  Artifact::checkFinalState(build, path);
}

// Commit any pending versions and save fingerprints for this artifact
void SymlinkArtifact::applyFinalState(fs::path path) noexcept {
  // Symlinks are always saved, so no need to fingerprint

  // Make sure this symlink is committed
  _symlink_version->commit(path);

  // Commit and fingerprint metadata
  Artifact::applyFinalState(path);
}

Resolution SymlinkArtifact::resolve(Build& build,
                                    shared_ptr<Command> c,
                                    shared_ptr<Artifact> prev,
                                    fs::path::iterator current,
                                    fs::path::iterator end,
                                    shared_ptr<Access> ref,
                                    bool committed) noexcept {
  auto& flags = ref->getFlags();

  // If requested, commit this artifact
  if (committed) commitAll();

  // If this is the end of the path and the nofollow flag is set, return this symlink
  if (current == end && flags.nofollow) return shared_from_this();

  // Otherwise we follow the symlink. That creates a path resolution dependency on our version
  build.observeInput(c, shared_from_this(), _symlink_version, InputType::PathResolution);

  // Get the symlink destination
  auto dest = _symlink_version->getDestination();

  // Append remaining path entries to the destination
  while (current != end) {
    dest /= *current++;
  }

  // Is the destination relative or absolute?
  if (dest.is_relative()) {
    // Resolve relative to the previous artifact, which must be the dir that holds this symlink
    return prev->resolve(build, c, nullptr, dest.begin(), dest.end(), ref, committed);

  } else {
    // Strip the leading slash from the path
    dest = dest.relative_path();

    // Resolve relative to root. First strip the leading slash off the path
    return _env->getRootDir()->resolve(build, c, nullptr, dest.begin(), dest.end(), ref, committed);
  }
}
