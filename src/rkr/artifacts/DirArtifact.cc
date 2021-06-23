#include "DirArtifact.hh"

#include <cerrno>
#include <filesystem>
#include <memory>
#include <sstream>
#include <string>
#include <tuple>

#include "artifacts/SymlinkArtifact.hh"
#include "data/AccessFlags.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "runtime/env.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirListVersion.hh"
#include "versions/DirVersion.hh"
#include "versions/MetadataVersion.hh"

using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tuple;

namespace fs = std::filesystem;

class Command;
class MetadataVersion;

DirArtifact::DirArtifact(MetadataVersion mv, shared_ptr<BaseDirVersion> dv) noexcept :
    Artifact(mv) {
  _base.update(dv);
  appendVersion(dv);
}

/// Revert this artifact to its committed state
void DirArtifact::rollback() noexcept {
  _base.rollback();

  for (const auto& [name, entry] : _entries) {
    entry->rollback();
  }

  Artifact::rollback();
}

/// Initialize this directory as an empty dir created by command c
void DirArtifact::createEmptyDir(std::shared_ptr<Command> c) noexcept {
  FAIL_IF(!c) << "A directory cannot be created by a null command";

  // Set up the base directory version
  auto v = make_shared<BaseDirVersion>(true);
  _base.update(c, v);
  appendVersion(v);

  // Add the base version to the command's outputs
  c->addDirectoryOutput(shared_from_this(), v);
}

/// Commit the content of this artifact to a specific path
void DirArtifact::commitContentTo(fs::path path) noexcept {
  // If the base version is committed there is no work to do
  if (_base.isCommitted()) return;

  // Make sure we have metadata for this uncommitted directory as well
  ASSERT(_metadata.isUncommitted()) << "Uncommitted directory does not have uncommitted metadata";

  // Get the base version and commit it with the appropriate metadata
  auto [version, _] = _base.getLatest();
  auto [metadata_version, __] = _metadata.getLatest();
  version->commit(path, metadata_version->getMode());

  // Mark the base version and metadata as committed
  _base.setCommitted();
  _metadata.setCommitted();
}

// Does this artifact have any uncommitted content?
bool DirArtifact::hasUncommittedContent() noexcept {
  return _base.isUncommitted();
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
    entry->commit();
  }
}

/// Commit a specific entry in this directory
void DirArtifact::commitEntry(string name) noexcept {
  auto iter = _entries.find(name);
  if (iter != _entries.end()) {
    iter->second->commit();
  }
}

/// Commit a link to this artifact at the given path
void DirArtifact::commitLink(shared_ptr<DirEntry> entry) noexcept {
  // Check for a matching committed link. If we find one, return.
  auto iter = _committed_links.find(entry);
  if (iter != _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = entry->getDir()->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto new_path = dir_path / entry->getName();

  // Three cases to handle for directories:
  // 1. The directory has a temporary path. Move it into place.
  // 2. The directory has another committed link already. This case isn't handled at the moment
  // 3. The directory has no committed links. Commit content to create one
  if (auto temp_path = takeTemporaryPath(); temp_path.has_value()) {
    // This artifact has a temporary path. We can move it to its new committed location
    LOG(artifact) << "Moving " << this << " from temporary location to " << new_path;

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
  _committed_links.emplace_hint(iter, entry);
  return;
}

/// Commit an unlink of this artifact at the given path
void DirArtifact::commitUnlink(shared_ptr<DirEntry> entry) noexcept {
  // Check for a matching committed link. If we don't find one, return immediately.
  auto iter = _committed_links.find(entry);
  if (iter == _committed_links.end()) return;

  // Get a path to the directory
  auto maybe_dir_path = entry->getDir()->commitPath();
  ASSERT(maybe_dir_path.has_value()) << "Committing link to a directory with no path";

  auto dir_path = maybe_dir_path.value();
  auto unlink_path = dir_path / entry->getName();

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

    if (rc != 0) {
      if (errno == ENOTEMPTY || errno == EEXIST) {
        // TODO: We really should not have this case. This is happening because the directory's
        // modeled state matches the committed state exactly, but we still need to commit it. There
        // are no unlinks in the output directory to commit, so the rmdir call fails.

        // WARN << "Failed to remove directory " << this << " from " << unlink_path
        //      << ". Cleaning up by force.";

        fs::remove_all(unlink_path);

      } else {
        FAIL << "Failed to unlink " << this << " from " << unlink_path << ": " << ERR;
      }
    }
  }

  // Remove the committed link and return
  _committed_links.erase(iter);
  return;
}

// Compare all final versions of this artifact to the filesystem state
void DirArtifact::checkFinalState(fs::path path) noexcept {
  // Recursively check the final state of all known entries
  for (const auto& [name, entry] : _entries) {
    // Get the targeted artifact
    auto artifact = entry->peekTarget();

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
  for (const auto& [name, entry] : _entries) {
    // Get the targeted artifact
    auto artifact = entry->peekTarget();

    // If there is a target, commit its final state
    if (artifact) artifact->applyFinalState(path / name);
  }
}

// Fingerprint and cache the committed state of this artifact
void DirArtifact::cacheAll(fs::path path) const noexcept {
  // Recursively cache all known entries
  for (const auto& [name, entry] : _entries) {
    // Get the targeted artifact
    auto artifact = entry->peekTarget();

    // If there is a target, commit its final state
    if (artifact) artifact->cacheAll(path / name);
  }
}

/// A traced command is about to (possibly) read from this artifact
void DirArtifact::beforeRead(Build& build, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Create a dependency on the content of this directory
  getContent(c);
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

  // Get the committed base version (if there is one)
  auto [committed_base, weak_committed_creator] = _base.getCommitted();

  // If this directory has committed state and was not created during the build, list it.
  // This is to handle the case where only a subset of a pre-existing directories entries are in the
  // artifact's entries map.
  if (committed_base && !weak_committed_creator.lock()) {
    // Get a committed path to this directory
    auto path = getCommittedPath();
    ASSERT(path.has_value()) << "Existing directory somehow has no committed path";

    for (auto& entry : fs::directory_iterator(path.value())) {
      auto name = entry.path().filename();
      if (name != ".rkr") {
        result->addEntry(name);
      }
    }
  }

  // Get the latest version
  auto [base, weak_creator] = _base.getLatest();
  auto creator = weak_creator.lock();

  // The command listing this directory depends on its base version
  if (c) c->addDirectoryInput(shared_from_this(), base, creator);

  for (const auto& [name, entry] : _entries) {
    // Get the artifact targeted by this entry. This access records a dependency on the entry.
    const auto& artifact = entry->getTarget(c);

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
    if (auto rc = flags.type.getResult(ArtifactType::Dir); rc != SUCCESS) {
      return rc;
    } else {
      return Ref(flags, shared_from_this());
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
    const auto& artifact = entries_iter->second->getTarget(c);

    // Is there an artifact to resolve to?
    if (artifact) {
      res = Ref(flags, artifact);
    } else {
      res = ENOENT;
    }

  } else {
    // Add a path resolution input from the base version
    auto [base, creator] = _base.getLatest();
    c->addDirectoryInput(shared_from_this(), base, creator.lock());

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
      auto entry_object = make_shared<DirEntry>(this->as<DirArtifact>(), entry);
      auto entry_version = make_shared<DirEntryVersion>(entry, artifact);
      appendVersion(entry_version);
      entry_object->setCommittedState(entry_version);
      _entries.emplace_hint(entries_iter, entry, entry_object);
    }
  }

  // We now have either a resolved artifact or an error code. The next step depends on whether
  // this is the last part of the path, or if there is more path left to resolve.
  if (current == end) {
    // This is the last entry in the resolution path

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
                           string name,
                           shared_ptr<Artifact> target) noexcept {
  // Make sure we have a record of this entry
  auto iter = _entries.find(name);
  if (iter == _entries.end()) {
    auto entry = make_shared<DirEntry>(this->as<DirArtifact>(), name);
    iter = _entries.emplace_hint(iter, name, entry);
  }

  // Create a version to represent this update
  auto version = make_shared<DirEntryVersion>(name, target);
  appendVersion(version);

  // Update the entry
  iter->second->updateEntry(c, version);
}

// Remove a directory entry from this artifact
void DirArtifact::removeEntry(const shared_ptr<Command>& c,
                              string name,
                              shared_ptr<Artifact> target) noexcept {
  // Make sure we have a record of this entry
  auto iter = _entries.find(name);
  if (iter == _entries.end()) {
    auto entry = make_shared<DirEntry>(this->as<DirArtifact>(), name);
    iter = _entries.emplace_hint(iter, name, entry);
  }

  // Create a version to represent this update
  auto version = make_shared<DirEntryVersion>(name, nullptr);
  appendVersion(version);

  // Update the entry
  iter->second->updateEntry(c, version);
}

DirEntry::DirEntry(shared_ptr<DirArtifact> dir, string name) noexcept : _dir(dir), _name(name) {}

// Set the committed state for this entry. Only used for initial state
void DirEntry::setCommittedState(std::shared_ptr<DirEntryVersion> version) noexcept {
  // Set the committed state of this entry
  _state.update(version);

  // If there is an initial target, inform it of its link
  if (auto target = version->getTarget(); target) {
    target->addLink(shared_from_this());
    target->addCommittedLink(shared_from_this());
  }
}

// Commit this entry's modeled state to the filesystem
void DirEntry::commit() noexcept {
  // If this entry has no uncommitted state, just return immediately
  if (_state.isCommitted()) return;

  // Is there an existing committed target?
  if (auto [v, _] = _state.getCommitted(); v && v->getTarget()) {
    v->getTarget()->commitUnlink(shared_from_this());
  }

  // Is there an uncommitted target? There may not be if this is just an unlink.
  if (auto [v, _] = _state.getUncommitted(); v && v->getTarget()) {
    v->getTarget()->commitLink(shared_from_this());
  }

  // The uncommitted state becomes the committed state
  _state.setCommitted();
}

/// Reset this entry to its committed state
void DirEntry::rollback() noexcept {
  // Get the committed and uncommitted versions of this entry
  auto [committed_v, _] = _state.getCommitted();
  auto [uncommitted_v, __] = _state.getUncommitted();

  // Is there an uncommitted version?
  if (uncommitted_v) {
    // If there is an uncommitted target, it loses this link
    if (uncommitted_v->getTarget()) uncommitted_v->getTarget()->removeLink(shared_from_this());

    // If there is a committed target, it regains an entry from this link
    if (committed_v && committed_v->getTarget()) {
      committed_v->getTarget()->addLink(shared_from_this());
    }
  }

  // If there is a committed target, roll it back
  if (committed_v && committed_v->getTarget()) {
    committed_v->getTarget()->rollback();
  }

  // Discard uncommitted state
  _state.rollback();
}

// Update this entry with a new version
void DirEntry::updateEntry(shared_ptr<Command> c, shared_ptr<DirEntryVersion> version) noexcept {
  // If there is uncommitted state and command c is running, commit first
  if (_state.isUncommitted() && c->mustRun()) commit();

  // Is there a version in place already? There may not be if this is a brand new entry.
  if (auto [v, writer] = _state.getLatest(); v) {
    // The command depends on the prior state of the entry
    c->addDirectoryInput(_dir.lock(), v, writer.lock());

    // If the latest version has a target artifact, that artifact is about to lose its link
    if (v->getTarget()) v->getTarget()->removeLink(shared_from_this());
  }

  // Record the version as output from command c
  c->addDirectoryOutput(_dir.lock(), version);

  // Now update the state with the new version
  if (c->mustRun()) {
    // The command is running or has already run, so all effects are automatically committed

    // Is there a committed target? If so, remove its committed link as well
    auto [committed_v, _] = _state.getCommitted();
    if (committed_v && committed_v->getTarget()) {
      committed_v->getTarget()->removeCommittedLink(shared_from_this());
    }

    // Inform the artifact of its new link
    auto target = version->getTarget();
    if (target) {
      target->addLink(shared_from_this());
      target->addCommittedLink(shared_from_this());
    }

    // Update the versioned state
    _state.update(c, version);

  } else {
    // Inform the artifact of its new link
    if (version->getTarget()) version->getTarget()->addLink(shared_from_this());

    // Update the versioned state
    _state.update(c, version);
  }
}

// Peek at the target of this entry without creating a dependency
shared_ptr<Artifact> DirEntry::peekTarget() const noexcept {
  auto [v, _] = _state.getLatest();
  return v->getTarget();
}

// Get the artifact linked at this entry on behalf of command c
shared_ptr<Artifact> DirEntry::getTarget(shared_ptr<Command> c) const noexcept {
  // Record the input to c, which may commit this entry
  auto [version, writer] = _state.getLatest();
  if (c) c->addDirectoryInput(_dir.lock(), version, writer.lock());

  return version->getTarget();
}
