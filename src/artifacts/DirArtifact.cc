#include "DirArtifact.hh"

#include <list>
#include <memory>
#include <string>
#include <utility>

#include <dirent.h>
#include <sys/types.h>
#include <unistd.h>

#include "interfaces/BuildObserver.hh"
#include "runtime/Build.hh"
#include "runtime/Env.hh"
#include "runtime/RefResult.hh"
#include "runtime/Resolution.hh"
#include "util/log.hh"
#include "versions/MetadataVersion.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tie;

DirArtifact::DirArtifact(shared_ptr<Env> env,
                         shared_ptr<MetadataVersion> mv,
                         shared_ptr<BaseDirVersion> dv) noexcept :
    Artifact(env, mv) {
  _base_dir_version = dv;
  appendVersion(dv);
}

bool DirArtifact::canCommit(shared_ptr<Version> v) const noexcept {
  if (auto dv = v->as<DirVersion>()) {
    return dv->canCommit();
  } else if (v == _metadata_version) {
    return _metadata_version->canCommit();
  } else {
    FAIL << "Attempted to check committable state for unknown version " << v << " in " << this;
    return false;
  }
}

void DirArtifact::commit(shared_ptr<Version> v) noexcept {
  // This directory must have some path, but it may not be committed yet.
  // If the path happens not to be committed, committing the base version should place it at that
  // path.

  // The base directory version must always be committed to commit any other version
  auto path = getPath(false);
  if (!path.has_value() && _link_updates.size() > 0) {
    auto [weak_dir, _, weak_version] = *_link_updates.begin();
    auto dir = weak_dir.lock();
    auto version = weak_version.lock();
    dir->commit(version);
    path = getPath(false);
  }
  ASSERT(path.has_value()) << "Committing to a directory with no path";
  _base_dir_version->commit(path.value());

  if (auto dv = v->as<DirVersion>()) {
    dv->commit(path.value());
  } else if (v == _metadata_version) {
    _metadata_version->commit(path.value());
  } else {
    FAIL << "Attempted to commit unknown version " << v << " in " << this;
  }
}

bool DirArtifact::canCommitAll() const noexcept {
  // Can the metadata version be committed?
  if (!_metadata_version->canCommit()) return false;

  // Can the base version be committed? If not, stop now.
  if (!_base_dir_version->canCommit()) return false;

  // Can every entry be committed?
  for (auto& [name, info] : _entries) {
    auto& [version, artifact] = info;
    if (!version->canCommit()) return false;
  }

  // Everything is committable
  return true;
}

// Commit all final versions of this artifact to the filesystem
void DirArtifact::commitAll() noexcept {
  // The base directory version must always be committed to commit any other version
  // Try to get a committed path to this directory
  auto path = getPath(false);
  if (!path.has_value() && _link_updates.size() > 0) {
    // If we don't have a committed path to this directory, commit one of its uncommitted paths
    auto [weak_dir, _, weak_version] = *_link_updates.begin();
    auto dir = weak_dir.lock();
    auto version = weak_version.lock();
    dir->commit(version);
    path = getPath(false);
  }
  ASSERT(path.has_value()) << "Committing to a directory with no path";
  _base_dir_version->commit(path.value());

  // Commit the versions needed for each entry
  for (auto& [name, info] : _entries) {
    auto& [version, artifact] = info;
    version->commit(path.value());
  }

  // Commit metadata
  _metadata_version->commit(path.value());
}

// Command c requires that this artifact exists in its current state. Create dependency edges.
void DirArtifact::mustExist(Build& build, shared_ptr<Command> c) noexcept {
  build.observeInput(c, shared_from_this(), _metadata_version, InputType::Exists);
  build.observeInput(c, shared_from_this(), _base_dir_version, InputType::Exists);

  for (auto [entry, info] : _entries) {
    auto [version, artifact] = info;
    build.observeInput(c, shared_from_this(), version, InputType::Exists);
  }
}

// Compare all final versions of this artifact to the filesystem state
void DirArtifact::checkFinalState(Build& build, fs::path path) noexcept {
  // Recursively check the final state of all known entries
  for (auto& [name, info] : _entries) {
    auto& [version, artifact] = info;

    // Do we expect the entry to point to an artifact?
    if (artifact) {
      // Yes. Make sure that artifact is in the expected final state
      artifact->checkFinalState(build, path / name);
    }

    // If the entry doesn't reference an artifact, we don't need to check for its absence. We only
    // have a record of this artifact being missing because some other part of the build accessed
    // it. That means there will be some earlier reference that was expected to succeed or fail
    // that will now have changed.
  }

  // Check the metadata state as well
  Artifact::checkFinalState(build, path);
}

// Commit any pending versions and save fingerprints for this artifact
void DirArtifact::applyFinalState(Build& build, fs::path path) noexcept {
  // First, commit this artifact and its metadata
  // TODO: Should we just commit the base version, then commit entries on demand?
  commitAll();

  // Fingerprint/commit any remaining metadata
  Artifact::applyFinalState(build, path);

  // Recursively apply final state for all known entries
  for (auto& [name, info] : _entries) {
    auto& [version, artifact] = info;
    if (artifact) artifact->applyFinalState(build, path / name);
  }
}

/// A traced command is about to (possibly) read from this artifact
void DirArtifact::beforeRead(Build& build,
                             shared_ptr<Command> c,
                             shared_ptr<RefResult> ref) noexcept {
  // Do nothing before a read
}

/// A traced command just read from this artifact
void DirArtifact::afterRead(Build& build,
                            shared_ptr<Command> c,
                            shared_ptr<RefResult> ref) noexcept {
  // The command now depends on the content of this directory
  build.traceMatchContent(c, ref, getList(build, c));
}

/// Check to see if this artifact's content matches a known version
void DirArtifact::matchContent(Build& build,
                               shared_ptr<Command> c,
                               shared_ptr<Version> expected) noexcept {
  // Get a list of entries in this directory
  auto observed = getList(build, c);

  // Compare the observed and expected versions
  if (!observed->matches(expected)) {
    // Report the mismatch
    build.observeMismatch(c, shared_from_this(), observed, expected);
  }
}

// Get a version that lists all the entries in this directory
shared_ptr<DirListVersion> DirArtifact::getList(Build& build, shared_ptr<Command> c) noexcept {
  // Create a DirListVersion to hold the list of directory entries
  auto result = make_shared<DirListVersion>();

  // If this directory was NOT created, get the list of entries from the filesystem
  if (!_base_dir_version->getCreated()) {
    // Get a path to the directory, but only allow committed paths
    auto path = getPath(false);
    ASSERT(path.has_value()) << "Existing directory somehow has no committed path";

    for (auto& entry : fs::directory_iterator(path.value())) {
      auto name = entry.path().stem();
      if (name != ".dodo") {
        result->addEntry(entry.path().stem());
      }
    }
  }

  // The command listing this directory depends on its base version
  build.observeInput(c, shared_from_this(), _base_dir_version, InputType::Accessed);

  for (auto [name, info] : _entries) {
    // Get the version and artifact for this entry
    auto [version, artifact] = info;

    // If this entry is from the base version, we've already covered it
    if (version == _base_dir_version) continue;

    // Otherwise the entry is from some other version. Update the list.
    if (artifact) {
      result->addEntry(name);
    } else {
      result->removeEntry(name);
    }

    // The listing command depends on whatever version is responsible for this entry
    build.observeInput(c, shared_from_this(), version, InputType::Accessed);
  }

  return result;
}

Resolution DirArtifact::resolve(Build& build,
                                shared_ptr<Command> c,
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
    if (!checkAccess(build, c, flags)) return EACCES;

    // The access was allowed. Did the access expect to reach a directory?
    if (flags.type == AccessType::Any || flags.type == AccessType::Dir) {
      return shared_from_this();
    } else {
      return EISDIR;
    }
  }

  // If the remaining path is not empty, make sure we have execute permission in this directory
  if (!checkAccess(build, c, AccessFlags{.x = true})) return EACCES;

  // We must be looking for an entry in this directory. Get the entry name and advance the
  // iterator
  auto entry = *current++;

  // Are we looking for the current directory?
  if (entry.string() == ".") {
    return resolve(build, c, shared_from_this(), current, end, flags, symlink_limit);
  }

  // Are we looking for the parent directory?
  if (entry.string() == "..") {
    auto parent = getParentDir();
    ASSERT(parent.has_value()) << "Directory has no parent";
    return parent.value()->resolve(build, c, shared_from_this(), current, end, flags,
                                   symlink_limit);
  }

  // We'll track the result of the resolution here
  Resolution res;

  // Check the map of known entries for a match
  auto entries_iter = _entries.find(entry);
  if (entries_iter != _entries.end()) {
    // Found a match.
    // Get the version responsible for this entry and the artifact it mapped (possibly null)
    auto [v, a] = entries_iter->second;

    // Is there an artifact to resolve to?
    if (a) {
      res = a;
    } else {
      res = ENOENT;
    }

    // Add a path resolution input from the version that matched
    build.observeInput(c, shared_from_this(), v, InputType::PathResolution);

  } else {
    // Add a path resolution input from the base version
    build.observeInput(c, shared_from_this(), _base_dir_version, InputType::PathResolution);

    // There's no match in the directory entry map. We need to check the base version for a match
    if (_base_dir_version->getCreated()) {
      // A created directory does not have any entries that don't appear in the map.
      res = ENOENT;

    } else {
      // Create a path to the entry. Start with a committed path to this directory
      auto dir_path = getPath(false);
      ASSERT(dir_path.has_value()) << "Directory has no path!";
      auto entry_path = dir_path.value() / entry;

      // Try to get the artifact from the filesystem
      auto artifact = getEnv()->getFilesystemArtifact(entry_path);

      // Did we get an artifact?
      if (artifact) {
        // Yes. Hang on to the result
        res = artifact;

        // Inform the artifact of its link in the current directory
        artifact->addLinkUpdate(this->as<DirArtifact>(), entry, _base_dir_version);

        // Add the entry to this directory's map
        _entries[entry] = {_base_dir_version, artifact};

      } else {
        // Record the absence of this entry and set the result to ENOENT
        _entries[entry] = {_base_dir_version, nullptr};
        res = ENOENT;
      }
    }
  }

  // We now have either a resolved artifact or an error code. The next step depends on whether
  // this is the last part of the path, or if there is more path left to resolve.
  if (current == end) {
    // This is the last entry in the resolution path

    // Was the reference required to create this entry?
    if (flags.create && flags.exclusive && res.isSuccess()) return EEXIST;

    // If the resolution failed, can this access create it?
    if (flags.create && res == ENOENT) {
      // Can we write to this directory? If not, return an error
      if (!checkAccess(build, c, AccessFlags{.w = true})) return EACCES;

      // Create a new file
      auto newfile = getEnv()->createFile(build, c, flags.mode, false);

      // Link the new file into this directory
      auto link_version = addEntry(build, c, entry, newfile);

      // return the artifact we just created and stop resolution
      return newfile;
    }

    // If the result was an error, return it
    if (!res.isSuccess()) return res;

    // Otherwise continue with resolution, which may follow symlinks
    return res.getArtifact()->resolve(build, c, shared_from_this(), current, end, flags,
                                      symlink_limit);

  } else {
    // There is still path left to resolve. Recursively resolve if the result succeeded
    if (res.isSuccess()) {
      return res.getArtifact()->resolve(build, c, shared_from_this(), current, end, flags,
                                        symlink_limit);
    }

    // Otherwise return the error from the resolution
    return res;
  }
}

// Add a directory entry to this artifact
shared_ptr<DirVersion> DirArtifact::addEntry(Build& build,
                                             shared_ptr<Command> c,
                                             fs::path entry,
                                             shared_ptr<Artifact> target) noexcept {
  // Check for an existing entry with the same name
  auto iter = _entries.find(entry);
  if (iter != _entries.end()) {
    // TODO: We will overwrite the old entry. How do we track that?
  }

  // Create a partial version to track the committed state of this entry
  auto writing = make_shared<AddEntry>(entry, target);
  writing->createdBy(c);

  // For this link to be committed, we need the artifact to exist or be committable
  target->mustExist(build, c);

  // Inform the artifact of its new link
  target->addLinkUpdate(as<DirArtifact>(), entry, writing);

  // Add the new entry to the entries map
  _entries[entry] = {writing, target};

  // Notify the build of this output
  build.observeOutput(c, shared_from_this(), writing);

  // Record this version in the artifact
  appendVersion(writing);

  return writing;
}

// Remove a directory entry from this artifact
shared_ptr<DirVersion> DirArtifact::removeEntry(Build& build,
                                                shared_ptr<Command> c,
                                                fs::path entry,
                                                shared_ptr<Artifact> target) noexcept {
  // Create a partial version to track the committed state of this update
  auto writing = make_shared<RemoveEntry>(entry, target);
  writing->createdBy(c);

  // Do we have a record of an entry with the given name?
  auto iter = _entries.find(entry);
  if (iter != _entries.end()) {
    // Get the version that added this entry, and the artifact it maps to
    auto& [version, artifact] = iter->second;

    // If there is an artifact at this entry, inform it of an unlink operation
    if (artifact) artifact->addLinkUpdate(as<DirArtifact>(), entry, writing);

    // Is the version that linked this entry uncommitted?
    if (!version->isCommitted()) {
      // Not committed. If the uncommitted version is an AddEntry version, we can cancel it out.
      if (auto add = version->as<AddEntry>()) {
        // The new RemoveEntry version cancels out the uncommitted AddEntry version
        add->setCommitted();
        writing->setCommitted();
      }
    }
  }

  // Update the entries map
  _entries[entry] = {writing, nullptr};

  // Notify the build of this output
  build.observeOutput(c, shared_from_this(), writing);

  // Record this version in the artifact as well
  appendVersion(writing);

  return writing;
}
