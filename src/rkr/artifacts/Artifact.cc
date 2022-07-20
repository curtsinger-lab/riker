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

using std::make_shared;
using std::map;
using std::nullopt;
using std::optional;
using std::shared_ptr;
using std::string;
using std::tuple;

Artifact::Artifact() noexcept {}

Artifact::Artifact(MetadataVersion v) noexcept {
  auto mv = make_shared<MetadataVersion>(v);
  appendVersion(mv);
  _metadata.update(mv);
}

string Artifact::getName() const noexcept {
  // If a fixed name was assigned, return it
  if (!_name.empty()) return _name;

  string shortest_name = "";

  // If not, build a name using the path to this artifact
  for (auto weak_entry : _modeled_links) {
    // Get the entry
    auto entry = weak_entry.lock();
    if (!entry) continue;

    // If there is no parent directory, this entry won't work
    auto parent = entry->getDir();
    if (!parent) continue;

    // If the parent directory does not have a name, this entry doesn't help
    auto parent_name = entry->getDir()->getName();
    if (parent_name.empty()) continue;

    // Generate a nice name for this artifact using the entry
    string new_name;
    if (parent_name == ".") {
      new_name = entry->getName();
    } else if (parent_name == "/") {
      new_name = "/" + entry->getName();
    } else {
      new_name = parent_name + "/" + entry->getName();
    }

    // Update the shortest name
    if (shortest_name.empty() || shortest_name.size() > new_name.size()) {
      shortest_name = new_name;
    }
  }

  return shortest_name;
}

void Artifact::rollback() noexcept {
  _metadata.rollback();
}

// Model a link to this artifact, but do not commit it to the filesystem
void Artifact::addLink(shared_ptr<DirEntry> entry) noexcept {
  // Warn if there is already a link at this path in the model
  auto iter = _modeled_links.find(entry);
  if (iter != _modeled_links.end()) {
    WARN << "Link {" << entry->getDir() << ", " << entry->getName() << "} already exists for "
         << this;
  }

  // Add the link
  _modeled_links.emplace_hint(iter, entry);
}

// Model an unlink of this artifact, but do not commit it to the filesystem
void Artifact::removeLink(shared_ptr<DirEntry> entry) noexcept {
  // Search for the link
  auto iter = _modeled_links.find(entry);
  if (iter == _modeled_links.end()) {
    WARN << "Link {" << entry->getDir() << ", " << entry->getName() << "} does not exist for "
         << this;
  } else {
    // Remove the link
    _modeled_links.erase(iter);
  }
}

// Add an already-committed link to this artifact
void Artifact::addCommittedLink(shared_ptr<DirEntry> entry) noexcept {
  // Make sure this is a link we already know about
  ASSERT(_modeled_links.find(entry) != _modeled_links.end())
      << "Adding a committed link to " << this
      << " that does not match a previously-known uncommitted link.";

  // Add this link to the set of committed links
  _committed_links.emplace(entry);
}

// Remove a committed link from this artifact
void Artifact::removeCommittedLink(shared_ptr<DirEntry> entry) noexcept {
  auto iter = _committed_links.find(entry);
  ASSERT(iter != _committed_links.end()) << "Removing a non-existant committed link to " << this;

  _committed_links.erase(iter);
}

// Get a committed path to this artifact
optional<fs::path> Artifact::getCommittedPath() const noexcept {
  if (_root_dir) return "/";

  for (auto weak_entry : _committed_links) {
    auto entry = weak_entry.lock();
    auto dir_path = entry->getDir()->getCommittedPath();
    if (dir_path.has_value()) return dir_path.value() / entry->getName();
  }
  return nullopt;
}

// Get a path to this artifact, either committed or uncommitted
optional<fs::path> Artifact::getPath() const noexcept {
  if (_root_dir) return "/";

  for (auto weak_entry : _modeled_links) {
    auto entry = weak_entry.lock();
    auto dir_path = entry->getDir()->getPath();
    if (dir_path.has_value()) return dir_path.value() / entry->getName();
  }
  return nullopt;
}

// Get a parent directory for this artifact. The result may or may not be on the filesystem
optional<shared_ptr<DirArtifact>> Artifact::getParentDir() noexcept {
  if (_modeled_links.empty()) {
    return nullopt;
  } else {
    auto weak_entry = *_modeled_links.begin();
    auto entry = weak_entry.lock();
    return entry->getDir();
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
  return getMetadata(c).checkAccess(flags);
}

optional<fs::path> Artifact::commitPath() noexcept {
  // Try to get a committed path to this artifact
  auto path = getCommittedPath();
  if (path.has_value()) return path;

  // If that didn't work, look for a link whose directory has a committed path and commit the link
  for (auto weak_entry : _modeled_links) {
    auto entry = weak_entry.lock();
    const auto& dir = entry->getDir();
    const auto& name = entry->getName();

    auto dir_path = dir->getCommittedPath();
    if (dir_path.has_value()) {
      dir->commitEntry(name);
      ASSERT(getCommittedPath().has_value()) << "Just committed link " << name << " to " << dir
                                             << ", but " << this << " still has no committed path";
      return dir_path.value() / name;
    }
  }

  // Finally, try to commit a directory's path for one of the uncommitted links
  for (auto weak_entry : _modeled_links) {
    auto entry = weak_entry.lock();
    const auto& dir = entry->getDir();
    const auto& name = entry->getName();

    auto dir_path = dir->commitPath();
    if (dir_path.has_value()) {
      dir->commitEntry(name);
      ASSERT(getCommittedPath().has_value()) << "Just committed link " << name << " to " << dir
                                             << ", but " << this << " still has no committed path";
      return dir_path.value() / name;
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
  if (path.has_value()) {
    commitContentTo(path.value());
  } else {
    // WARN << "Committing content to an artifact with no path";
  }
}

void Artifact::commitMetadata() noexcept {
  // If metadata is already committed, there's nothing to do
  if (_metadata.isCommitted()) return;

  // Get a committed path to this artifact, possibly by committing links above it in the path
  auto path = commitPath();
  ASSERT(path.has_value()) << "Committing metadata to an artifact with no path";

  commitMetadataTo(path.value());
}

void Artifact::commitMetadataTo(fs::path path) noexcept {
  // If metadata is already committed, there's nothing to do
  if (_metadata.isCommitted()) return;

  auto [version, writer] = _metadata.getLatest();
  version->commit(path);
  _metadata.setCommitted();
}

// Commit any pending versions and save fingerprints for this artifact
void Artifact::applyFinalState(fs::path path) noexcept {
  commitMetadataTo(path);
}

/// Get the current metadata version for this artifact
MetadataVersion Artifact::getMetadata(const shared_ptr<Command>& c) noexcept {
  // Get the current metadata and writer and notify the reader of this input
  auto [version, writer] = _metadata.getLatest();
  if (c) c->addMetadataInput(shared_from_this(), version, writer);

  // If c is running, commit the metadata
  if (c && c->mustRun()) commitMetadata();

  return *version;
}

/// Get the current metadata for this artifact without creating any dependencies
MetadataVersion Artifact::peekMetadata() noexcept {
  return getMetadata(nullptr);
}

/// Check to see if this artifact's metadata matches a known version
void Artifact::matchMetadata(const shared_ptr<Command>& c,
                             Scenario scenario,
                             MetadataVersion expected) noexcept {
  // Get the current metadata
  auto observed = getMetadata(c);

  // Compare versions
  if (!observed.matches(expected)) {
    // Report the mismatch
    LOGF(artifact, "Metadata mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, observed);
    c->inputChanged(shared_from_this(), observed, expected, scenario);
  }
}

/// Apply a new metadata version to this artifact
void Artifact::updateMetadata(const shared_ptr<Command>& c, MetadataVersion writing) noexcept {
  auto mv = make_shared<MetadataVersion>(writing);
  appendVersion(mv);
  _metadata.update(c, mv);

  // Report the output to the build
  c->addMetadataOutput(shared_from_this(), mv);
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
    if (auto rc = flags.type.getResult(ArtifactType::File); rc != SUCCESS) {
      return rc;
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

  open_flags |= O_CLOEXEC;  // Open all FDs as close-on-exec
  open_flags &= ~O_TRUNC;   // The opened file should not be truncated

  // Open the artifact in cloexec mode so children do not inherit it
  int fd = ::open(path.value().c_str(), open_flags, open_mode);
  FAIL_IF(fd < 0) << "Failed to open " << this;

  return fd;
}
