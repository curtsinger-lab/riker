#include "Artifact.hh"

// IWYU pragma: no_include <type_traits>
#include <cerrno>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <tuple>

#include <fcntl.h>

#include "artifacts/DirArtifact.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "runtime/env.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirVersion.hh"
#include "versions/MetadataVersion.hh"

using std::make_shared;
using std::map;
using std::nullopt;
using std::optional;
using std::set;
using std::shared_ptr;
using std::string;
using std::tuple;

Artifact::Artifact(shared_ptr<MetadataVersion> v) noexcept {
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

void Artifact::setCommitted() noexcept {
  _metadata_version->setCommitted();
  for (auto& [weak_dir, entry, weak_version] : _link_updates) {
    auto version = weak_version.lock();
    if (version) version->setCommitted();
  }
}

void Artifact::addLinkUpdate(shared_ptr<DirArtifact> dir,
                             fs::path entry,
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
optional<fs::path> Artifact::getPath(bool allow_uncommitted) const noexcept {
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

  // If the caller does not want an uncommitted path, return an empty optional
  if (!allow_uncommitted) return nullopt;

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

/// Get a parent directory for this artifact. The result may or may not be on the filesystem
optional<shared_ptr<DirArtifact>> Artifact::getParentDir() noexcept {
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
  return this->as<DirArtifact>();
}

// Generate and save a temporary path for this artifact. Returns the new path.
// The caller must make sure this artifact is linked at the new temporary path.
fs::path Artifact::assignTemporaryPath() noexcept {
  ASSERT(!_temp_path.has_value())
      << "Cannot assign a temporary path to an artifact that already has one";

  auto path = env::getTempPath();
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
bool Artifact::checkAccess(const shared_ptr<Command>& c, AccessFlags flags) noexcept {
  c->currentRun()->addMetadataInput(shared_from_this(), InputType::PathResolution);
  return _metadata_version->checkAccess(shared_from_this(), flags);
}

optional<fs::path> Artifact::commitPath() noexcept {
  // Get a committed path to this artifact
  auto path = getPath(false);
  if (!path.has_value() && _link_updates.size() > 0) {
    // If we don't have a committed path to this artifact, commit one of its uncommitted paths
    auto [weak_dir, _, weak_version] = *_link_updates.begin();
    auto dir = weak_dir.lock();
    auto version = weak_version.lock();
    dir->commit(version);
    path = getPath(false);
  }
  return path;
}

void Artifact::commitMetadata(optional<fs::path> path) noexcept {
  LOG(artifact) << "Committing metadata to " << this;

  // Get a committed path to this artifact, possibly by committing links above it in the path
  if (!path.has_value()) path = commitPath();
  ASSERT(path.has_value()) << "Committing metadata to an artifact with no path";

  _metadata_version->commit(path.value());
}

// Commit any pending versions and save fingerprints for this artifact
void Artifact::applyFinalState(fs::path path) noexcept {
  // Make sure metadata for this artifact is committed
  _metadata_version->commit(path);
}

/// Get the current metadata version for this artifact
shared_ptr<MetadataVersion> Artifact::getMetadata(const shared_ptr<Command>& c,
                                                  InputType t) noexcept {
  // Notify the build of the input
  if (c) c->currentRun()->addMetadataInput(shared_from_this(), t);

  // Return the metadata version
  return _metadata_version;
}

/// Get the current metadata for this artifact without creating any dependencies
shared_ptr<MetadataVersion> Artifact::peekMetadata() noexcept {
  return getMetadata(nullptr, InputType::Accessed);
}

/// Check to see if this artifact's metadata matches a known version
void Artifact::matchMetadata(const shared_ptr<Command>& c,
                             Scenario scenario,
                             shared_ptr<MetadataVersion> expected) noexcept {
  // Get the current metadata
  auto observed = getMetadata(c, InputType::Accessed);

  // Compare versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    LOGF(artifact, "Metadata mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, observed);
    c->currentRun()->inputChanged(shared_from_this(), observed, expected, scenario);
  }
}

/// Apply a new metadata version to this artifact
shared_ptr<MetadataVersion> Artifact::updateMetadata(const shared_ptr<Command>& c,
                                                     shared_ptr<MetadataVersion> writing) noexcept {
  ASSERT(writing) << "Attempted to write a null metadata version to " << this;

  // Update the metadata version for this artifact
  appendVersion(writing);
  _metadata_version = writing;

  // Report the output to the build
  c->currentRun()->addMetadataOutput(shared_from_this(), _metadata_version);

  return writing;
}

void Artifact::appendVersion(shared_ptr<MetadataVersion> v) noexcept {
  _metadata_versions.push_back(v);
}

void Artifact::appendVersion(shared_ptr<ContentVersion> v) noexcept {
  _content_versions.push_back(v);
}

Ref Artifact::resolve(const shared_ptr<Command>& c,
                      shared_ptr<Artifact> prev,
                      fs::path::iterator current,
                      fs::path::iterator end,
                      AccessFlags flags,
                      size_t symlink_limit) noexcept {
  // Are we at the end of the path to resolve?
  if (current == end) {
    // Check to see if the requested access mode is supported
    if (!checkAccess(c, flags)) return EACCES;

    // Access is allowed. Did the access expect a specific type of artifact?
    if (flags.type == AccessType::Dir) {
      return ENOTDIR;
    } else if (flags.type == AccessType::Symlink) {
      return EINVAL;
    } else {
      return Ref(flags, shared_from_this());
    }
  }

  return ENOTDIR;
}

int Artifact::getFD(AccessFlags flags) noexcept {
  // Get a path to this artifact
  auto path = getPath();
  ASSERT(path.has_value()) << "Cannot open artifact without a path";

  // Get flags to pass to the open call
  auto [open_flags, open_mode] = flags.toOpen();

  // Open the artifact
  int fd = ::open(path.value().c_str(), open_flags, open_mode);
  FAIL_IF(fd < 0) << "Failed to open " << this;

  return fd;
}
