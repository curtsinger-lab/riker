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
#include "versions/DirVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::make_shared;
using std::nullopt;
using std::set;
using std::shared_ptr;

Artifact::Artifact(shared_ptr<Env> env, shared_ptr<MetadataVersion> v) noexcept : _env(env) {
  appendVersion(v);
  _metadata_version = v;
}

string Artifact::getName() const noexcept {
  // If a fixed name was assigned, return it
  if (!_name.empty()) return _name;

  // Get all the committed an uncommitted links to this artifact
  auto [committed_links, uncommitted_links] = getLinks();

  // Try to get a name from the committed links first
  for (auto [link, version] : committed_links) {
    auto [dir, entry] = link;
    return (fs::path(dir->getName()) / entry).lexically_normal();
  }

  // Fall back to uncommitted links
  for (auto& [link, version] : uncommitted_links) {
    auto [dir, entry] = link;
    return (fs::path(dir->getName()) / entry).lexically_normal();
  }

  // Otherwise return an empty name
  return string();
}

void Artifact::addLinkUpdate(shared_ptr<DirArtifact> dir,
                             string entry,
                             shared_ptr<DirVersion> v) noexcept {
  // Record the link update
  // TODO: We can cancel out old updates that have been overwritten by newer committed updates.
  _link_updates.emplace_back(dir, entry, v);
}

tuple<map<Artifact::Link, shared_ptr<DirVersion>>, map<Artifact::Link, shared_ptr<DirVersion>>>
Artifact::getLinks() const noexcept {
  map<Link, shared_ptr<DirVersion>> committed;
  map<Link, shared_ptr<DirVersion>> uncommitted;

  // Loop over the sequence of link updates
  for (auto& [weak_dir, entry, weak_version] : _link_updates) {
    auto dir = weak_dir.lock();
    auto version = weak_version.lock();

    // First, check to see if we were not given a version; this happens only for root.
    // Treat this case as a committed link
    if (!version) {
      committed.emplace(Link{dir, entry}, version);

    } else if (auto unlink = version->as<RemoveEntry>()) {
      // This is an unlink. Is it committed or not?
      if (unlink->isCommitted()) {
        // A committed unlink removes any committed links
        committed.erase({dir, entry});

        // There should be no previous uncommitted links that this committed unlink matches
        ASSERT(uncommitted.find({dir, entry}) == uncommitted.end())
            << "Committed unlink " << unlink << " matches an uncommitted link to " << this;

      } else {
        // An uncommitted unlink removes only uncommitted links
        uncommitted.erase({dir, entry});

        // if there is a committed link to this same directory, it continues to exist until the
        // unlink is committed
      }

    } else {
      // The version is not an unlink. Add the link the appropriate map
      if (version->isCommitted()) {
        committed.emplace(Link{dir, entry}, version);
      } else {
        uncommitted.emplace(Link{dir, entry}, version);
      }
    }
  }

  return {committed, uncommitted};
}

/// Get a path to this artifact that may or may not be committed to the filesystem
optional<fs::path> Artifact::getPath() const noexcept {
  // Get all the links to this artifact, both committed and uncommitted
  auto [committed_links, uncommitted_links] = getLinks();

  // Try to construct a committed path first
  for (auto [link, version] : committed_links) {
    auto [dir, entry] = link;

    // Check for a null parent directory, which should only happen for root
    if (!dir) return entry;

    auto dir_path = dir->getPath();
    if (dir_path.has_value()) {
      return dir_path.value() / entry;
    }
  }

  // Fall back to an uncommitted path
  for (auto [link, version] : uncommitted_links) {
    auto [dir, entry] = link;

    // Check for a null parent directory, which should only happen for root
    if (!dir) return entry;

    auto dir_path = dir->getPath();
    if (dir_path.has_value()) {
      return dir_path.value() / entry;
    }
  }

  // There is no known path to this artifact
  return nullopt;
}

/// Get a path to this artifact that is currently committed to the filesystem
optional<fs::path> Artifact::getCommittedPath() const noexcept {
  // Get all the links to this artifact, both committed and uncommitted
  auto [committed_links, uncommitted_links] = getLinks();

  // Try to construct a committed path
  for (auto [link, version] : committed_links) {
    auto [dir, entry] = link;

    // Check for a null parent directory, which should only happen for root
    if (!dir) return entry;

    auto dir_path = dir->getCommittedPath();
    if (dir_path.has_value()) {
      return dir_path.value() / entry;
    }
  }

  // There is no committed path to this artifact
  return nullopt;
}

/// Get a parent directory for this artifact. The result may or may not be on the filesystem
optional<shared_ptr<DirArtifact>> Artifact::getParentDir() const noexcept {
  // Get all the links to this artifact, both committed and uncommitted
  auto [committed_links, uncommitted_links] = getLinks();

  // Look for a committed parent directory first
  for (auto [link, version] : committed_links) {
    auto [dir, entry] = link;
    if (dir) return dir;
  }

  // Fall back to an uncommitted parent directory
  for (auto [link, version] : uncommitted_links) {
    auto [dir, entry] = link;
    if (dir) return dir;
  }

  // This artifact has no known parent directory
  return nullopt;
}

// Generate and save a temporary path for this artifact. Returns the new path.
// The caller must make sure this artifact is linked at the new temporary path.
fs::path Artifact::assignTemporaryPath() noexcept {
  ASSERT(!_temp_path.has_value())
      << "Cannot assign a temporary path to an artifact that already has one";

  auto path = _env->getTempPath();
  _temp_path = path;
  return path;
}

// Clear the temporary path for this artifact. Returns the old temporary path if it had one.
optional<fs::path> Artifact::takeTemporaryPath() noexcept {
  optional<fs::path> path;
  std::swap(_temp_path, path);
  return path;
}

// Check if an access is allowed by the metadata for this artifact
bool Artifact::checkAccess(Build& build, shared_ptr<Command> c, AccessFlags flags) noexcept {
  build.observeInput(c, shared_from_this(), _metadata_version, InputType::PathResolution);
  return _metadata_version->checkAccess(shared_from_this(), flags);
}

// Can a specific version of this artifact be committed?
bool Artifact::canCommit(shared_ptr<Version> v) const noexcept {
  ASSERT(v == _metadata_version) << "Called canCommit with unknown version on artifact " << this;
  return _metadata_version->canCommit();
}

// Commit a specific version of this artifact to the filesystem
void Artifact::commit(shared_ptr<Version> v) noexcept {
  ASSERT(v == _metadata_version) << "Called commit with unknown version on artifact " << this;
  auto path = getCommittedPath();
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
void Artifact::checkFinalState(Build& build, fs::path path) noexcept {
  if (!_metadata_version->isCommitted()) {
    auto v = make_shared<MetadataVersion>();
    v->fingerprint(path);

    // Is there a difference between the tracked version and what's on the filesystem?
    if (!_metadata_version->matches(v)) {
      // Yes. Report the mismatch
      build.observeFinalMismatch(shared_from_this(), _metadata_version, v);
    } else {
      // No. We can treat the metadata version as if it is committed
      _metadata_version->setCommitted();
    }
  }
}

// Commit any pending versions and save fingerprints for this artifact
void Artifact::applyFinalState(fs::path path) noexcept {
  // If we don't have a fingerprint of the metadata, take one
  if (!_metadata_version->hasFingerprint()) {
    ASSERT(_metadata_version->isCommitted()) << "Cannot fingerprint an uncommitted version";
    _metadata_version->fingerprint(path);
  }

  // Make sure metadata for this artifact is committed
  _metadata_version->commit(path);
}

/// Get the current metadata version for this artifact
shared_ptr<MetadataVersion> Artifact::getMetadata(Build& build,
                                                  shared_ptr<Command> c,
                                                  InputType t) noexcept {
  // Notify the build of the input
  build.observeInput(c, shared_from_this(), _metadata_version, t);

  // Return the metadata version
  return _metadata_version;
}

/// Check to see if this artifact's metadata matches a known version
void Artifact::match(Build& build,
                     shared_ptr<Command> c,
                     shared_ptr<MetadataVersion> expected) noexcept {
  // Get the current metadata
  auto observed = getMetadata(build, c, InputType::Accessed);

  // Compare versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    build.observeMismatch(c, shared_from_this(), observed, expected);
  }
}

/// Apply a new metadata version to this artifact
void Artifact::apply(Build& build,
                     shared_ptr<Command> c,
                     shared_ptr<MetadataVersion> writing) noexcept {
  // Update the metadata version for this artifact
  appendVersion(writing);
  _metadata_version = writing;

  // Report the output to the build
  build.observeOutput(c, shared_from_this(), _metadata_version);
}

void Artifact::appendVersion(shared_ptr<Version> v) noexcept {
  _versions.push_back(v);
}

Resolution Artifact::resolve(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<Artifact> prev,
                             fs::path::iterator current,
                             fs::path::iterator end,
                             shared_ptr<Access> ref,
                             bool committed) noexcept {
  // Are we at the end of the path to resolve?
  if (current == end) {
    // Check to see if the requested access mode is supported
    if (!checkAccess(build, c, ref->getFlags())) return EACCES;
    if (committed) commitAll();
    return shared_from_this();
  }

  return ENOTDIR;
}

/// Specialize get for metadata
template <>
shared_ptr<MetadataVersion> Artifact::get<MetadataVersion>(Build& build,
                                                           shared_ptr<Command> c,
                                                           InputType t) {
  return getMetadata(build, c, t);
}

/// Specialize get for content
template <>
shared_ptr<FileVersion> Artifact::get<FileVersion>(Build& build,
                                                   shared_ptr<Command> c,
                                                   InputType t) {
  return getContent(build, c, t);
}

/// Specialize get for symlink
template <>
shared_ptr<SymlinkVersion> Artifact::get<SymlinkVersion>(Build& build,
                                                         shared_ptr<Command> c,
                                                         InputType t) {
  return getSymlink(build, c, t);
}

/// Specialize get for directory list
template <>
shared_ptr<ListedDir> Artifact::get<ListedDir>(Build& build, shared_ptr<Command> c, InputType t) {
  return getDirList(build, c, t);
}
