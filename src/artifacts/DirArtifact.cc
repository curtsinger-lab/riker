#include "DirArtifact.hh"

#include <cerrno>
#include <filesystem>
#include <memory>
#include <sstream>
#include <string>
#include <tuple>

#include "data/AccessFlags.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "runtime/env.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirListVersion.hh"
#include "versions/DirVersion.hh"

using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tuple;

namespace fs = std::filesystem;

class Command;
class MetadataVersion;

DirArtifact::DirArtifact(shared_ptr<MetadataVersion> mv, shared_ptr<BaseDirVersion> dv) noexcept :
    Artifact(mv) {
  _committed_base_version = dv;
  appendVersion(dv);
}

/// Initialize this directory as an empty dir created by command c
void DirArtifact::createEmptyDir(std::shared_ptr<Command> c) noexcept {
  FAIL_IF(!c) << "A directory cannot be created by a null command";

  // Record the command that created this directory
  _creator = c;

  // Set up the base directory version
  auto base = make_shared<BaseDirVersion>(true);
  if (c->running() || c->alreadyRun()) {
    _committed_base_version = base;
  } else {
    _uncommitted_base_version = base;
  }

  // Add the base version to the creating command's outputs
  c->addDirectoryOutput(shared_from_this(), base);

  appendVersion(base);
}

const shared_ptr<BaseDirVersion>& DirArtifact::getBaseVersion() const noexcept {
  if (_uncommitted_base_version) return _uncommitted_base_version;
  return _committed_base_version;
}

/// Commit the content of this artifact to a specific path
void DirArtifact::commitContentTo(fs::path path) noexcept {
  if (_uncommitted_base_version) {
    _uncommitted_base_version->commit(path);
    _committed_base_version = nullptr;
    std::swap(_uncommitted_base_version, _committed_base_version);
  }
}

// Does this artifact have any uncommitted content?
bool DirArtifact::hasUncommittedContent() noexcept {
  if (_uncommitted_base_version) return true;
  return false;
}

// Commit all final versions of this artifact to the filesystem
void DirArtifact::commitAll() noexcept {
  // Get a committed path to this directory
  auto path = commitPath();
  ASSERT(path.has_value()) << "Committing to a directory with no path";

  // Commit the base directory version
  commitContentTo(path.value());

  // Commit metadata
  commitMetadataTo(path.value());

  // Commit each entry in this directory
  for (auto& [name, entry] : _entries) {
    entry.commit(this->as<DirArtifact>(), name);
  }
}

/// Commit a specific entry in this directory
void DirArtifact::commitEntry(fs::path name) noexcept {
  auto iter = _entries.find(name);
  if (iter != _entries.end()) {
    iter->second.commit(this->as<DirArtifact>(), name);
  }
}

/// Commit a link to this artifact at the given path
void DirArtifact::commitLink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  LOG(artifact) << "Committing link to " << this << " at " << dir << " entry " << entry;

  // Check for a matching committed link. If we find one, return.
  auto iter = _committed_links.find(Link{dir, entry});
  if (iter != _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = dir->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto new_path = dir_path / entry;

  // Three cases to handle for directories:
  // 1. The directory has a temporary path. Move it into place.
  // 2. The directory has another committed link already. This case isn't handled at the moment
  // 3. The directory has no committed links. Commit content to create one
  if (auto temp_path = takeTemporaryPath(); temp_path.has_value()) {
    // This artifact has a temporary path. We can move it to its new committed location
    LOG(artifact) << "Moving " << this << " from temporary location to " << dir_path / entry;

    // Yes. Move the artifact into place
    int rc = ::rename(temp_path.value().c_str(), new_path.c_str());
    FAIL_IF(rc != 0) << "Failed to move " << this << " from a temporary location: " << ERR;

  } else if (auto committed_path = getCommittedPath(); committed_path.has_value()) {
    // The symlink has an existing committed path. Bail on this for now.
    FAIL << "Artifact " << this << " already has a committed path, and cannot be hard linked.";

  } else {
    ASSERT(hasUncommittedContent()) << "Artifact has no committed path, but content is committed";

    // This artifact has no paths. Create one by committing its content.
    commitContentTo(new_path);
  }

  // Record the committed link
  _committed_links.emplace_hint(iter, Link{dir, entry});
  return;
}

/// Commit an unlink of this artifact at the given path
void DirArtifact::commitUnlink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  LOG(artifact) << "Committing unlink of " << this << " at " << dir << " entry " << entry;
  // Check for a matching committed link. If we don't find one, return immediately.
  auto iter = _committed_links.find(Link{dir, entry});
  if (iter == _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = dir->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto unlink_path = dir_path / entry;

  // Committing an unlink of a directory has two cases:
  // 1. There are uncommitted links, but no other committed links. Move to a temporary path.
  // 2. Otherwise commit all entries (which probably unlink files) and then remove it
  if (_committed_links.size() == 1 && _modeled_links.size() > 0) {
    LOG(artifact) << "Unlinking " << this << " at " << unlink_path << ": move to temporary path";

    // Get a temporary path for this file
    auto temp_path = assignTemporaryPath();

    // Move the file
    int rc = ::rename(unlink_path.c_str(), temp_path.c_str());
    FAIL_IF(rc != 0) << "Failed to move " << this << " to a temporary location: " << ERR;

  } else {
    LOG(artifact) << "Unlinking " << this << " at " << unlink_path << ": remove directory";

    // Commit the directory's content, which should remove all of its entries
    commitAll();

    // Now remove the directory
    int rc = ::rmdir(unlink_path.c_str());
    FAIL_IF(rc != 0) << "Failed to unlink " << this << " from " << unlink_path << ": " << ERR;
  }

  // Remove the committed link and return
  _committed_links.erase(iter);
  return;
}

// Compare all final versions of this artifact to the filesystem state
void DirArtifact::checkFinalState(fs::path path) noexcept {
  // Recursively check the final state of all known entries
  for (auto& [name, entry] : _entries) {
    // Get the targeted artifact
    auto artifact = entry.peekTarget();

    // If there is a target, make sure that artifact is in the expected final state
    if (artifact) artifact->checkFinalState(path / name);

    // If the entry doesn't reference an artifact, we don't need to check for its absence. We only
    // have a record of this artifact being missing because some other part of the build accessed
    // it. That means there will be some earlier reference that was expected to succeed or fail
    // that will now have changed.
  }
}

// Commit any pending versions and save fingerprints for this artifact
void DirArtifact::applyFinalState(fs::path path) noexcept {
  // First, commit this artifact and its metadata
  // TODO: Should we just commit the base version, then commit entries on demand?
  commitAll();

  // Fingerprint/commit any remaining metadata
  Artifact::applyFinalState(path);

  // Recursively apply final state for all known entries
  for (auto& [name, entry] : _entries) {
    // Get the targeted artifact
    auto artifact = entry.peekTarget();

    // If there is a target, commit its final state
    if (artifact) artifact->applyFinalState(path / name);
  }
}

/// A traced command is about to (possibly) read from this artifact
void DirArtifact::beforeRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void DirArtifact::afterRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The command now depends on the content of this directory
  build.traceMatchContent(c, ref, getContent(c));
}

// Get a version that lists all the entries in this directory
shared_ptr<ContentVersion> DirArtifact::getContent(const shared_ptr<Command>& c) noexcept {
  // Create a DirListVersion to hold the list of directory entries
  auto result = make_shared<DirListVersion>();

  // If this directory was NOT created, get the list of entries from the filesystem
  if (_committed_base_version && !_committed_base_version->getCreated()) {
    // Get a committed path to this directory
    auto path = getCommittedPath();
    ASSERT(path.has_value()) << "Existing directory somehow has no committed path";

    for (auto& entry : fs::directory_iterator(path.value())) {
      auto name = entry.path().stem();
      if (name != ".rkr") {
        result->addEntry(entry.path().stem());
      }
    }
  }

  const auto& base = getBaseVersion();

  // The command listing this directory depends on its base version
  if (c) c->addDirectoryInput(shared_from_this(), base, _creator, InputType::Accessed);

  for (const auto& [name, entry] : _entries) {
    // Get the artifact targeted by this entry. This access records a dependency on the entry.
    const auto& artifact = entry.getTarget(c, this->as<DirArtifact>());

    // Does the entry target an artifact?
    if (artifact) {
      result->addEntry(name);
    } else {
      result->removeEntry(name);
    }
  }

  return result;
}

/// Check to see if this artifact's content matches a known version
void DirArtifact::matchContent(const shared_ptr<Command>& c,
                               Scenario scenario,
                               shared_ptr<ContentVersion> expected) noexcept {
  // Get a list of entries in this directory
  auto observed = getContent(c);

  // Compare the observed and expected versions
  if (!observed->matches(expected)) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, observed);
    // Report the mismatch
    c->inputChanged(shared_from_this(), observed, expected, scenario);
  }
}

Ref DirArtifact::resolve(const shared_ptr<Command>& c,
                         shared_ptr<Artifact> prev,
                         fs::path::iterator current,
                         fs::path::iterator end,
                         AccessFlags flags,
                         size_t symlink_limit) noexcept {
  // If the path has a trailing slash, the final entry will be empty. Advance past any empty
  // entries
  while (current != end && current->empty()) current++;

  // If this is the last entry on the path, resolution reaches this artifact
  if (current == end) {
    // If the requested access is not allowed, return EACCES
    if (!checkAccess(c, flags)) return EACCES;

    // The access was allowed. Did the access expect to reach a directory?
    if (flags.type == AccessType::Any || flags.type == AccessType::Dir) {
      return Ref(flags, shared_from_this());
    } else {
      return EISDIR;
    }
  }

  // If the remaining path is not empty, make sure we have execute permission in this directory
  if (!checkAccess(c, ExecAccess)) return EACCES;

  // We must be looking for an entry in this directory. Get the entry name and advance the
  // iterator
  const auto& entry = *current++;

  // Are we looking for the current directory?
  auto entry_str = entry.string();

  if (entry_str == ".") {
    return resolve(c, shared_from_this(), current, end, flags, symlink_limit);
  }

  // Are we looking for the parent directory?
  if (entry_str == "..") {
    const auto& parent = getParentDir();
    ASSERT(parent.has_value()) << "Directory has no parent";
    return parent.value()->resolve(c, shared_from_this(), current, end, flags, symlink_limit);
  }

  // We'll track the result of the resolution here
  Ref res;

  // Check the map of known entries for a match
  auto entries_iter = _entries.find(entry);
  if (entries_iter != _entries.end()) {
    // Found a match.

    // Get the target of this entry. This access creates a dependency on the entry's state
    const auto& artifact = entries_iter->second.getTarget(c, this->as<DirArtifact>());

    // Is there an artifact to resolve to?
    if (artifact) {
      res = Ref(flags, artifact);
    } else {
      res = ENOENT;
    }

  } else {
    // Add a path resolution input from the base version
    const auto& base = getBaseVersion();
    c->addDirectoryInput(shared_from_this(), base, _creator, InputType::PathResolution);

    // There's no match in the directory entry map. We need to check the base version for a match
    if (base->getCreated()) {
      // A created directory does not have any entries that don't appear in the map.
      res = ENOENT;

    } else {
      // Create a path to the entry. Start with a committed path to this directory
      auto dir_path = getCommittedPath();
      ASSERT(dir_path.has_value()) << "Directory has no path!";
      auto entry_path = dir_path.value() / entry;

      // Try to get the artifact from the filesystem
      const auto& artifact = env::getFilesystemArtifact(entry_path);

      // Did we get an artifact?
      if (artifact) {
        // Yes. Hang on to the result
        res = Ref(flags, artifact);

      } else {
        // Set the result to ENOENT
        res = ENOENT;
      }

      // Add the entry to this directory's map of entries
      _entries.emplace_hint(entries_iter, entry,
                            Entry(this->as<DirArtifact>(), entry, artifact, base));
    }
  }

  // We now have either a resolved artifact or an error code. The next step depends on whether
  // this is the last part of the path, or if there is more path left to resolve.
  if (current == end) {
    // This is the last entry in the resolution path

    // Was the artifact opened O_NOFOLLOW and is it a symlink?
    // if (flags.nofollow && res.getArtifact()->getTypeName() == "Symlink") {
    //   return ELOOP;
    // }

    // Was the reference required to create this entry?
    if (flags.create && flags.exclusive && res.getResultCode() == SUCCESS) return EEXIST;

    // If the resolution failed, can this access create it?
    if (flags.create && res.getResultCode() == ENOENT) {
      // Can we write to this directory? If not, return an error
      if (!checkAccess(c, WriteAccess)) return EACCES;

      // Create a new file
      const auto& newfile = env::createFile(c, flags.mode);

      // Link the new file into this directory
      addEntry(c, entry, newfile);

      // return the artifact we just created and stop resolution
      return Ref(flags, newfile);
    }

    // If the result was an error, return it
    if (res.getResultCode() != SUCCESS) return res;

    // Otherwise continue with resolution, which may follow symlinks
    return res.getArtifact()->resolve(c, shared_from_this(), current, end, flags, symlink_limit);

  } else {
    // There is still path left to resolve. Recursively resolve if the result succeeded
    if (res.isSuccess()) {
      return res.getArtifact()->resolve(c, shared_from_this(), current, end, flags, symlink_limit);
    }

    // Otherwise return the error from the resolution
    return res;
  }
}

// Add a directory entry to this artifact
void DirArtifact::addEntry(const shared_ptr<Command>& c,
                           fs::path entry,
                           shared_ptr<Artifact> target) noexcept {
  // Update the target of the named entry
  auto written = _entries[entry].updateTarget(c, target, this->as<DirArtifact>(), entry);

  // Record this version in the artifact
  appendVersion(written);
}

// Remove a directory entry from this artifact
void DirArtifact::removeEntry(const shared_ptr<Command>& c,
                              fs::path entry,
                              shared_ptr<Artifact> target) noexcept {
  // Update the target of the named entry
  auto written = _entries[entry].updateTarget(c, nullptr, this->as<DirArtifact>(), entry);

  // Record this version in the artifact
  appendVersion(written);
}

DirArtifact::Entry::Entry(shared_ptr<DirArtifact> dir,
                          fs::path name,
                          shared_ptr<Artifact> target,
                          shared_ptr<DirVersion> version) noexcept :
    _committed_target(target), _committed_version(version) {
  // If there is an initial target, inform it of its link
  if (target) {
    target->addLink(dir, name);
    target->addCommittedLink(dir, name);
  }
}

// Commit this entry's modeled state to the filesystem
void DirArtifact::Entry::commit(shared_ptr<DirArtifact> dir, fs::path name) noexcept {
  // If this entry has no uncommitted state, just return immediately
  if (!_uncommitted_version) return;

  // Is there an existing committed target?
  if (_committed_target) {
    _committed_target->commitUnlink(dir, name);
  }

  // Is there an uncommitted target? There may not be if this is just an unlink.
  if (_uncommitted_target) {
    _uncommitted_target->commitLink(dir, name);
  }

  // The uncommitted state becomes the committed state
  _committed_target = std::move(_uncommitted_target);
  _committed_version = std::move(_uncommitted_version);
}

// Update this entry to reach a new target artifact on behalf of a command
shared_ptr<DirVersion> DirArtifact::Entry::updateTarget(shared_ptr<Command> c,
                                                        shared_ptr<Artifact> target,
                                                        shared_ptr<DirArtifact> dir,
                                                        fs::path name) noexcept {
  // If there is uncommitted state and command c is running, commit first
  if (_uncommitted_version && c->running()) commit(dir, name);

  // First, create an input to command c from the current version
  if (_uncommitted_version) {
    c->addDirectoryInput(dir, _uncommitted_version, _writer.lock(), InputType::Accessed);
  } else {
    c->addDirectoryInput(dir, _committed_version, _writer.lock(), InputType::Accessed);
  }

  // Next, update records of uncommitted links
  if (_uncommitted_version) {
    // The uncommitted target is going to lose its link (if there is one)
    if (_uncommitted_target) _uncommitted_target->removeLink(dir, name);
  } else {
    // The committed target loses its link (if there is one)
    if (_committed_target) _committed_target->removeLink(dir, name);
  }

  // Create a version to represent the update
  shared_ptr<DirVersion> version;
  if (target) {
    version = make_shared<AddEntry>(name);
  } else {
    version = make_shared<RemoveEntry>(name);
  }

  // Record the version as output from command c
  c->addDirectoryOutput(dir, version);

  // Now update the state with the new version
  if (c->running() || c->alreadyRun()) {
    // The command is running or has already run, so all effects are automatically committed

    // Is there a committed target? If so, remove its committed link as well
    if (_committed_target) _committed_target->removeCommittedLink(dir, name);

    // Inform the artifact of its new link
    if (target) {
      target->addLink(dir, name);
      target->addCommittedLink(dir, name);
    }

    // There is no longer an uncommitted target or version
    _uncommitted_target.reset();
    _uncommitted_version.reset();

    // Update the committed target and version
    _committed_target = target;
    _committed_version = version;

  } else {
    // Inform the artifact of its new link
    if (target) target->addLink(dir, name);

    // Update the uncommitted state. All links have already been updated
    _uncommitted_target = target;
    _uncommitted_version = version;
  }

  // Remember that command c last wrote this entry
  _writer = c;

  // Return the version that was just written to this entry
  return version;
}

// Peek at the target of this entry without creating a dependency
const shared_ptr<Artifact>& DirArtifact::Entry::peekTarget() const noexcept {
  if (_uncommitted_version) return _uncommitted_target;
  return _committed_target;
}

// Get the artifact linked at this entry on behalf of command c
const shared_ptr<Artifact>& DirArtifact::Entry::getTarget(
    shared_ptr<Command> c,
    shared_ptr<DirArtifact> dir) const noexcept {
  // Does this entry have an uncommitted update?
  if (_uncommitted_version) {
    // Yes. Record the command's input
    if (c) c->addDirectoryInput(dir, _uncommitted_version, _writer.lock(), InputType::Accessed);

    // Return the uncommitted target
    return _uncommitted_target;

  } else {
    // No. Use the committed state
    if (c) c->addDirectoryInput(dir, _committed_version, _writer.lock(), InputType::Accessed);

    // Return the committed target
    return _committed_target;
  }
}
