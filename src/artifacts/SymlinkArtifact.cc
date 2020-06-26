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
                                                       shared_ptr<Reference> ref,
                                                       InputType t) noexcept {
  // Mark the metadata as accessed
  _symlink_version->accessed();

  // Notify the build of the input
  _env.getBuild().observeInput(c, ref, shared_from_this(), _symlink_version, t);

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
void SymlinkArtifact::commit(fs::path path) noexcept {
  _symlink_version->commit(path);
  Artifact::commit(path);
}

// Compare all final versions of this artifact to the filesystem state
void SymlinkArtifact::checkFinalState(fs::path path) noexcept {
  if (!_symlink_version->isCommitted()) {
    // TODO: Compare to on-disk symlink state here
  }

  // Check the metadata state as well
  Artifact::checkFinalState(path);
}

// Take fingerprints for all final versions of this artifact
void SymlinkArtifact::fingerprintFinalState(fs::path path) noexcept {
  // Symlinks are fully saved, so just call up to the artifact for metadata
  Artifact::fingerprintFinalState(path);
}

// Check to see if this artifact's symlink destination matches a known version
void SymlinkArtifact::match(shared_ptr<Command> c,
                            shared_ptr<Reference> ref,
                            shared_ptr<SymlinkVersion> expected) noexcept {
  // Get the current metadata
  auto observed = getSymlink(c, ref, InputType::Accessed);

  // Compare versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    _env.getBuild().observeMismatch(c, shared_from_this(), observed, expected);
  }
}

Resolution SymlinkArtifact::resolve(shared_ptr<Command> c,
                                    fs::path resolved,
                                    fs::path remaining,
                                    shared_ptr<Access> ref,
                                    bool committed) noexcept {
  auto& flags = ref->getFlags();

  // If requested, commit this artifact
  if (committed) commit(resolved);

  // If this is the end of the path and the nofollow flag is set, return this symlink
  if (remaining.empty() && flags.nofollow) return shared_from_this();

  // Otherwise we follow the symlink. That creates a path resolution dependency on our version
  //_env.getBuild().observeInput(c, self, shared_from_this(), _symlink_version,
  //                             InputType::PathResolution);

  // Get the symlink destination
  auto dest = _symlink_version->getDestination();

  // Is the symlink destination relative or absolute?
  if (dest.is_absolute()) {
    // Strip the leading slash off the destination
    dest = dest.relative_path();

    // Now resolve relative to the root directory
    return _env.getRootDir()->resolve(c, "/", dest / remaining, ref, committed);

  } else {
    // If the destination is relative, resolution starts in this symlink's parent directory
    auto parent_path = (resolved / "..").lexically_normal();
    auto parent = _env.getPath(parent_path);
    ASSERT(parent) << "Failed to resolve parent directory";
    return parent->resolve(c, parent_path, dest / remaining, ref, committed);
  }
}
