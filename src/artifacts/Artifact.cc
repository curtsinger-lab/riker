#include "Artifact.hh"

#include <memory>
#include <set>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "build/AccessTypes.hh"
#include "build/Build.hh"
#include "build/Env.hh"
#include "core/Command.hh"
#include "core/IR.hh"
#include "ui/options.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::make_shared;
using std::nullopt;
using std::set;
using std::shared_ptr;

Artifact::Artifact(Env& env, shared_ptr<MetadataVersion> v) noexcept : _env(env) {
  appendVersion(v);
  _metadata_version = v;
}

string Artifact::getName() const noexcept {
  // If a fixed name was assigned, return it
  if (!_name.empty()) return _name;

  // Try to construct a name from the committed links to this artifact
  for (auto& [dir, entry] : _committed_links) {
    return (fs::path(dir->getName()) / entry).lexically_normal();
  }

  // Try an uncommitted link
  for (auto& [dir, entry] : _uncommitted_links) {
    return (fs::path(dir->getName()) / entry).lexically_normal();
  }

  // Otherwise return an empty name
  return string();
}

/// Notify this artifact that it is linked to a parent directory with a given entry name.
/// If committed is true, the link is already in place on the filesystem.
void Artifact::linkAt(shared_ptr<DirArtifact> dir, string entry, bool committed) noexcept {
  if (committed) {
    _committed_links.emplace(dir.get(), entry);
  } else {
    _uncommitted_links.emplace(dir.get(), entry);
  }
}

/// Update the filesystem so this artifact is linked in the given directory
void Artifact::commitLinkAt(shared_ptr<DirArtifact> dir, string entry) noexcept {
  FAIL << "commitLinkAt() function is not implemented";
}

/// Notify this artifact that it is unlinked from ap arent directory at a given entry name.
/// If committed is true, the link has already been removed on the filesystem.
void Artifact::unlinkAt(shared_ptr<DirArtifact> dir, string entry, bool committed) noexcept {
  if (committed) {
    _committed_links.erase(tuple{dir.get(), entry});
  } else {
    _uncommitted_links.erase(tuple{dir.get(), entry});
  }
}

/// Update the filesystem so this artifact is no longer linked in the given directory
void Artifact::commitUnlinkAt(shared_ptr<DirArtifact> dir, string entry) noexcept {
  FAIL << "commitUnlinkAt() function is not implemented";
}

/// Get a reasonable path to this artifact. The path may not by in place on the filesystem, but
/// the path will reflect a location of this artifact at some point during the build.
optional<fs::path> Artifact::getPath() const noexcept {
  // Try to get a committed path first
  auto result = getCommittedPath();
  if (result.has_value()) return result;

  // Fall back on uncommitted paths
  for (auto& [dir, name] : _uncommitted_links) {
    // Check for a null parent directory, which should only happen for root
    if (dir == nullptr) return name;

    auto dir_path = dir->getPath();
    if (dir_path.has_value()) return dir_path.value() / name;
  }

  // No paths?
  return nullopt;
}

/// Get a committed path to this artifact. The path may be a temporary location that does not
/// appear during the build, but this artifact is guaranteed to be at that path.
optional<fs::path> Artifact::getCommittedPath() const noexcept {
  // TODO: Check for temporary location

  // Get a committed path
  for (auto& [dir, name] : _committed_links) {
    // Check for a null parent directory, which should only happen for root
    if (dir == nullptr) return name;

    auto dir_path = dir->getPath();
    if (dir_path.has_value()) return dir_path.value() / name;
  }

  // No committed path
  return nullopt;
}

/// Get a parent directory for this artifact. The result may or may not be on the filesystem
optional<DirArtifact*> Artifact::getParentDir() const noexcept {
  if (_committed_links.size() > 0) {
    auto [parent, name] = *_committed_links.begin();
    return parent;
  }

  if (_uncommitted_links.size() > 0) {
    auto [parent, name] = *_uncommitted_links.begin();
    return parent;
  }

  return nullopt;
}

// Check if an access is allowed by the metadata for this artifact
bool Artifact::checkAccess(shared_ptr<Command> c, AccessFlags flags) noexcept {
  _env.getBuild().observeInput(c, shared_from_this(), _metadata_version, InputType::PathResolution);

  // If the current metadata version is committed, make sure we save it for future checks
  // if (_metadata_version->isCommitted()) _metadata_version->save(ref);
  return _metadata_version->checkAccess(flags);
}

// Can a specific version of this artifact be committed?
bool Artifact::canCommit(shared_ptr<Version> v) const noexcept {
  ASSERT(v == _metadata_version) << "Called canCommit with unknown version on artifact " << this;
  return _metadata_version->canCommit();
}

// Commit a specific version of this artifact to the filesystem
void Artifact::commit(shared_ptr<Version> v) noexcept {
  ASSERT(v == _metadata_version) << "Called commit with unknown version on artifact " << this;
  auto path = getPath();
  ASSERT(path.has_value()) << "Artifact has no path";
  _metadata_version->commit(path.value());
}

// Can this artifact be fully committed?
bool Artifact::canCommitAll() const noexcept {
  return canCommit(_metadata_version);
}

// Commit all final versions of this artifact to the filesystem
void Artifact::commitAll() noexcept {
  commit(_metadata_version);
}

// Compare all final versions of this artifact to the filesystem state
void Artifact::checkFinalState() noexcept {
  auto path = getPath();
  ASSERT(path.has_value()) << "Artifact has no path";

  if (!_metadata_version->isCommitted()) {
    auto v = make_shared<MetadataVersion>();
    v->fingerprint(path.value());

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_metadata_version->matches(v)) {
      // Yes. Report the mismatch
      _env.getBuild().observeFinalMismatch(shared_from_this(), _metadata_version, v);
    } else {
      // No. We can treat the metadata version as if it is committed
      _metadata_version->setCommitted();
    }
  }
}

// Commit any pending versions and save fingerprints for this artifact
void Artifact::applyFinalState() noexcept {
  auto path = getPath();
  ASSERT(path.has_value()) << "Artifact has no path";

  // If we don't have a fingerprint of the metadata, take one
  if (!_metadata_version->hasFingerprint()) {
    ASSERT(_metadata_version->isCommitted()) << "Cannot fingerprint an uncommitted version";
    _metadata_version->fingerprint(path.value());
  }

  // Make sure metadata for this artifact is committed
  _metadata_version->commit(path.value());
}

/// Get the current metadata version for this artifact
shared_ptr<MetadataVersion> Artifact::getMetadata(shared_ptr<Command> c, InputType t) noexcept {
  // Notify the build of the input
  _env.getBuild().observeInput(c, shared_from_this(), _metadata_version, t);

  // Return the metadata version
  return _metadata_version;
}

/// Check to see if this artifact's metadata matches a known version
void Artifact::match(shared_ptr<Command> c, shared_ptr<MetadataVersion> expected) noexcept {
  // Get the current metadata
  auto observed = getMetadata(c, InputType::Accessed);

  // Compare versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    _env.getBuild().observeMismatch(c, shared_from_this(), observed, expected);
  }
}

/// Apply a new metadata version to this artifact
void Artifact::apply(shared_ptr<Command> c, shared_ptr<MetadataVersion> writing) noexcept {
  // Update the metadata version for this artifact
  appendVersion(writing);
  _metadata_version = writing;

  // Report the output to the build
  _env.getBuild().observeOutput(c, shared_from_this(), _metadata_version);
}

void Artifact::appendVersion(shared_ptr<Version> v) noexcept {
  _versions.push_back(v);
}

Resolution Artifact::resolve(shared_ptr<Command> c,
                             shared_ptr<Artifact> prev,
                             fs::path::iterator current,
                             fs::path::iterator end,
                             shared_ptr<Access> ref,
                             bool committed) noexcept {
  // Are we at the end of the path to resolve?
  if (current == end) {
    // Check to see if the requested access mode is supported
    if (!checkAccess(c, ref->getFlags())) return EACCES;
    if (committed) commitAll();
    return shared_from_this();
  }

  return ENOTDIR;
}
