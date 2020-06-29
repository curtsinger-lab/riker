#include "SymlinkArtifact.hh"

#include <memory>

#include "build/Build.hh"
#include "versions/SymlinkVersion.hh"

using std::shared_ptr;

SymlinkArtifact::SymlinkArtifact(Env& env,
                                 shared_ptr<MetadataVersion> mv,
                                 shared_ptr<SymlinkVersion> sv) noexcept :
    Artifact(env, mv) {
  appendVersion(sv);
  _symlink_version = sv;
}

// Get the current symlink version of this artifact
shared_ptr<SymlinkVersion> SymlinkArtifact::getSymlink(shared_ptr<Command> c,
                                                       InputType t) noexcept {
  // Notify the build of the input
  _env.getBuild().observeInput(c, shared_from_this(), _symlink_version, t);

  // Return the metadata version
  return _symlink_version;
}

bool SymlinkArtifact::canCommit() const noexcept {
  return Artifact::canCommit();
}

bool SymlinkArtifact::isCommitted() const noexcept {
  return _symlink_version->isCommitted() && Artifact::isCommitted();
}

// Commit all final versions of this artifact to the filesystem
void SymlinkArtifact::commit() noexcept {
  auto path = getPath();
  ASSERT(!path.empty()) << "Symlink has no path";

  _symlink_version->commit(path);
  Artifact::commit();
}

// Compare all final versions of this artifact to the filesystem state
void SymlinkArtifact::checkFinalState() noexcept {
  if (!_symlink_version->isCommitted()) {
    // TODO: Compare to on-disk symlink state here
  }

  // Check the metadata state as well
  Artifact::checkFinalState();
}

// Commit any pending versions and save fingerprints for this artifact
void SymlinkArtifact::applyFinalState() noexcept {
  // Symlinks are always saved, so no need to fingerprint

  auto path = getPath();
  ASSERT(!path.empty()) << "Symlink has no path";

  // Make sure this symlink is committed
  _symlink_version->commit(path);

  // Commit and fingerprint metadata
  Artifact::applyFinalState();
}

// Check to see if this artifact's symlink destination matches a known version
void SymlinkArtifact::match(shared_ptr<Command> c, shared_ptr<SymlinkVersion> expected) noexcept {
  // Get the current metadata
  auto observed = getSymlink(c, InputType::Accessed);

  // Compare versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    _env.getBuild().observeMismatch(c, shared_from_this(), observed, expected);
  }
}

Resolution SymlinkArtifact::resolve(shared_ptr<Command> c,
                                    shared_ptr<Artifact> prev,
                                    fs::path remaining,
                                    shared_ptr<Access> ref,
                                    bool committed) noexcept {
  auto& flags = ref->getFlags();

  // If requested, commit this artifact
  if (committed) commit();

  // If this is the end of the path and the nofollow flag is set, return this symlink
  if (remaining.empty() && flags.nofollow) return shared_from_this();

  // Otherwise we follow the symlink. That creates a path resolution dependency on our version
  _env.getBuild().observeInput(c, shared_from_this(), _symlink_version, InputType::PathResolution);

  // Get the symlink destination
  auto dest = _symlink_version->getDestination();

  // Is the destination relative or absolute?
  if (dest.is_relative()) {
    // Resolve relative to the previous artifact, which must be the dir that holds this symlink
    return prev->resolve(c, nullptr, dest / remaining, ref, committed);

  } else {
    // Resolve relative to root. First strip the leading slash off the path
    return _env.getRootDir()->resolve(c, nullptr, dest.relative_path() / remaining, ref, committed);
  }
}
