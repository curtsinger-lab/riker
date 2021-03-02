#include "Artifact.hh"

#include <cerrno>
#include <map>
#include <memory>
#include <optional>
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
#include "versions/PipeVersion.hh"

using std::map;
using std::nullopt;
using std::optional;
using std::shared_ptr;
using std::string;
using std::tuple;

Artifact::Artifact() noexcept {}

Artifact::Artifact(shared_ptr<MetadataVersion> v) noexcept {
  appendVersion(v);
  _committed_metadata = v;
}

string Artifact::getName() const noexcept {
  // If a fixed name was assigned, return it
  if (!_name.empty()) return _name;

  // Get a path and return it, or an empty string if there is no path to this artifact
  auto path = getPath();
  return path.value_or("");
}

// Model a link to this artifact, but do not commit it to the filesystem
void Artifact::addLink(shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  // Warn if there is already a link at this path in the model
  if (_modeled_links.find({dir, entry}) != _modeled_links.end()) {
    WARN << "Link {" << dir << ", " << entry << "} already exists for " << this;
  }

  // Add the link
  _modeled_links.emplace(dir, entry);
}

// Model an unlink of this artifact, but do not commit it to the filesystem
void Artifact::removeLink(shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  // Search for the link
  auto iter = _modeled_links.find({dir, entry});
  if (iter == _modeled_links.end()) {
    WARN << "Link {" << dir << ", " << entry << "} does not exist for " << this;
  } else {
    // Remove the link
    _modeled_links.erase(iter);
  }
}

// Add an already-committed link to this artifact
void Artifact::addCommittedLink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  // Make sure this is a link we already know about
  ASSERT(_modeled_links.find(Link{dir, entry}) != _modeled_links.end())
      << "Adding a committed link to " << this
      << " that does not match a previously-known uncommitted link.";

  // Add this link to the set of committed links
  _committed_links.emplace(dir, entry);
}

// Remove a committed link from this artifact
void Artifact::removeCommittedLink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  _committed_links.erase({dir, entry});
}

// Get a committed path to this artifact
optional<fs::path> Artifact::getCommittedPath() const noexcept {
  for (const auto& [dir, entry] : _committed_links) {
    // The root directory is its own parent directory
    if (entry.empty() && dir == this->as<DirArtifact>()) return "/";

    auto dir_path = dir->getCommittedPath();
    if (dir_path.has_value()) return dir_path.value() / entry;
  }
  return nullopt;
}

// Get a path to this artifact, either committed or uncommitted
optional<fs::path> Artifact::getPath() const noexcept {
  for (const auto& [dir, entry] : _modeled_links) {
    // The root directory is its own parent directory
    if (entry.empty() && dir == this->as<DirArtifact>()) return "/";

    auto dir_path = dir->getPath();
    if (dir_path.has_value()) return dir_path.value() / entry;
  }
  return nullopt;
}

// Get a parent directory for this artifact. The result may or may not be on the filesystem
optional<shared_ptr<DirArtifact>> Artifact::getParentDir() noexcept {
  if (_modeled_links.empty()) {
    return nullopt;
  } else {
    const auto& [dir, entry] = *_modeled_links.begin();
    return dir;
  }
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
  return getMetadata(c, InputType::PathResolution)->checkAccess(shared_from_this(), flags);
}

optional<fs::path> Artifact::commitPath() noexcept {
  // Try to get a committed path to this artifact
  auto path = getCommittedPath();
  if (path.has_value()) return path;

  // If that didn't work, look for a link whose directory has a committed path and commit the link
  for (const auto& [dir, entry] : _modeled_links) {
    auto dir_path = dir->getCommittedPath();
    if (dir_path.has_value()) {
      commitLink(dir, entry);
      ASSERT(getCommittedPath().has_value()) << "Just committed link " << entry << " to " << dir
                                             << ", but " << this << " still has no committed path";
      return dir_path.value() / entry;
    }
  }

  // Finally, try to commit a directory's path for one of the uncommitted links
  for (const auto& [dir, entry] : _modeled_links) {
    auto dir_path = dir->commitPath();
    if (dir_path.has_value()) {
      commitLink(dir, entry);
      ASSERT(getCommittedPath().has_value()) << "Just committed link " << entry << " to " << dir
                                             << ", but " << this << " still has no committed path";
      return dir_path.value() / entry;
    }
  }

  return nullopt;
}

/// Commit the content of this artifact to a specific path
void Artifact::commitContent() noexcept {
  // If this artifact's content is fully committed, stop immediately
  if (!hasUncommittedContent()) return;

  // Get a committed path to this artifact, possibly by committing links above it in the path
  auto path = commitPath();
  ASSERT(path.has_value()) << "Committing content to an artifact with no path";

  commitContentTo(path.value());
}

void Artifact::commitMetadata() noexcept {
  if (!_uncommitted_metadata) return;

  // Get a committed path to this artifact, possibly by committing links above it in the path
  auto path = commitPath();
  ASSERT(path.has_value()) << "Committing metadata to an artifact with no path";

  commitMetadataTo(path.value());
}

void Artifact::commitMetadataTo(fs::path path) noexcept {
  if (!_uncommitted_metadata) return;

  // If this artifact still has uncommitted metadata, commit it
  if (_uncommitted_metadata) {
    _uncommitted_metadata->commit(path);
    _committed_metadata = std::move(_uncommitted_metadata);
  }
}

void Artifact::setMetadataCommitted() noexcept {
  if (_uncommitted_metadata) _committed_metadata = std::move(_uncommitted_metadata);
}

// Commit any pending versions and save fingerprints for this artifact
void Artifact::applyFinalState(fs::path path) noexcept {
  commitMetadataTo(path);
}

/// Get the current metadata version for this artifact
shared_ptr<MetadataVersion> Artifact::getMetadata(const shared_ptr<Command>& c,
                                                  InputType t) noexcept {
  auto result = _committed_metadata;
  if (_uncommitted_metadata) result = _uncommitted_metadata;

  ASSERT(result) << "Artifact " << this << " has no metadata version";

  // Notify the build of the input
  if (c) {
    c->addMetadataInput(shared_from_this(), result, _metadata_writer.lock(), t);
  }

  return result;
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
    c->inputChanged(shared_from_this(), observed, expected, scenario);
  }
}

/// Apply a new metadata version to this artifact
void Artifact::updateMetadata(const shared_ptr<Command>& c,
                              shared_ptr<MetadataVersion> writing) noexcept {
  ASSERT(writing) << "Attempted to write a null metadata version to " << this;

  // Remember which command wrote the metadata
  _metadata_writer = c;

  // Remember this metadata version
  appendVersion(writing);

  // Is the writing command running?
  if (c->running()) {
    // Yes. The metadata is committed
    _uncommitted_metadata.reset();
    _committed_metadata = writing;
  } else {
    // No. The metadata is uncommitted
    _uncommitted_metadata = writing;
  }

  // Report the output to the build
  c->addMetadataOutput(shared_from_this(), writing);
}

void Artifact::appendVersion(shared_ptr<Version> v) noexcept {
  if (options::track_inputs_outputs) _versions.push_back(v);
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

  // Open the artifact in cloexec mode so children do not inherit it
  int fd = ::open(path.value().c_str(), open_flags | O_CLOEXEC, open_mode);
  FAIL_IF(fd < 0) << "Failed to open " << this;

  return fd;
}
