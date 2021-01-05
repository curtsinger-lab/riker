#include "DirArtifact.hh"

#include <list>
#include <memory>
#include <string>
#include <utility>

#include <dirent.h>
#include <sys/types.h>
#include <unistd.h>

#include "runtime/Build.hh"
#include "runtime/Ref.hh"
#include "runtime/env.hh"
#include "util/log.hh"
#include "versions/MetadataVersion.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tie;

DirArtifact::DirArtifact(shared_ptr<MetadataVersion> mv, shared_ptr<BaseDirVersion> dv) noexcept :
    Artifact(mv) {
  _base_dir_version = dv;
  appendVersion(dv);
}

bool DirArtifact::canCommit(shared_ptr<ContentVersion> v) const noexcept {
  auto dv = v->as<DirVersion>();
  ASSERT(dv) << "Attempted to check committable state for unknown version " << v << " in " << this;
  return dv->canCommit();
}

void DirArtifact::commit(shared_ptr<ContentVersion> v) noexcept {
  // Get a committed path to this artifact, possibly by committing links above it in the path
  auto path = commitPath();

  LOG(artifact) << "Committing content version " << v << " to " << this;

  if (_base_dir_version->isCommitted()) {
    LOG(artifact) << "  base dir already committed at " << path.value();
  }

  _base_dir_version->commit(path.value());

  auto dv = v->as<DirVersion>();
  ASSERT(dv) << "Attempted to commit unknown version " << v << " in " << this;
  dv->commit(path.value());
}

bool DirArtifact::canCommitAll() const noexcept {
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
void DirArtifact::commitAll(optional<fs::path> path) noexcept {
  LOG(artifact) << "Committing content and metadata to " << this;

  // If we weren't given a specific path to commit to, get one by committing links
  if (!path.has_value()) path = commitPath();

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

// Commit the minimal set of versions requires to ensure this artifact exists on the filesystem
void DirArtifact::commitMinimal(fs::path path) noexcept {
  LOG(artifact) << "Committing minimal content and metadata to " << this;
  _base_dir_version->commit(path);
  _metadata_version->commit(path);
}

// Compare all final versions of this artifact to the filesystem state
void DirArtifact::checkFinalState(fs::path path) noexcept {
  // Recursively check the final state of all known entries
  for (auto& [name, info] : _entries) {
    auto& [version, artifact] = info;

    // Do we expect the entry to point to an artifact?
    if (artifact) {
      // Yes. Make sure that artifact is in the expected final state
      artifact->checkFinalState(path / name);
    }

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
  for (auto& [name, info] : _entries) {
    auto& [version, artifact] = info;
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
  build.traceMatchContent(c, ref, getList(c));
}

// Get this artifact's content without creating dependencies
shared_ptr<ContentVersion> DirArtifact::peekContent() noexcept {
  return getList(nullptr);
}

/// Check to see if this artifact's content matches a known version
void DirArtifact::matchContent(const shared_ptr<Command>& c,
                               Scenario scenario,
                               shared_ptr<ContentVersion> expected) noexcept {
  // Get a list of entries in this directory
  auto observed = getList(c);

  // Compare the observed and expected versions
  if (!observed->matches(expected)) {
    LOGF(artifact, "Content mismatch in {} ({} scenario {}): \n  expected {}\n  observed {}", this,
         c, scenario, expected, observed);
    // Report the mismatch
    c->currentRun()->inputChanged(shared_from_this(), observed, expected, scenario);
  }
}

// Get a version that lists all the entries in this directory
shared_ptr<DirListVersion> DirArtifact::getList(const shared_ptr<Command>& c) noexcept {
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
  if (c)
    c->currentRun()->addContentInput(shared_from_this(), _base_dir_version, InputType::Accessed);

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
    if (c) c->currentRun()->addContentInput(shared_from_this(), version, InputType::Accessed);
  }

  return result;
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
  auto entry = *current++;

  // Are we looking for the current directory?
  if (entry.string() == ".") {
    return resolve(c, shared_from_this(), current, end, flags, symlink_limit);
  }

  // Are we looking for the parent directory?
  if (entry.string() == "..") {
    auto parent = getParentDir();
    ASSERT(parent.has_value()) << "Directory has no parent";
    return parent.value()->resolve(c, shared_from_this(), current, end, flags, symlink_limit);
  }

  // We'll track the result of the resolution here
  Ref res;

  // Check the map of known entries for a match
  auto entries_iter = _entries.find(entry);
  if (entries_iter != _entries.end()) {
    // Found a match.
    // Get the version responsible for this entry and the artifact it mapped (possibly null)
    auto [v, a] = entries_iter->second;

    // Is there an artifact to resolve to?
    if (a) {
      res = Ref(flags, a);
    } else {
      res = ENOENT;
    }

    // Add a path resolution input from the version that matched
    c->currentRun()->addContentInput(shared_from_this(), v, InputType::PathResolution);

  } else {
    // Add a path resolution input from the base version
    c->currentRun()->addContentInput(shared_from_this(), _base_dir_version,
                                     InputType::PathResolution);

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
      auto artifact = env::getFilesystemArtifact(entry_path);

      // Did we get an artifact?
      if (artifact) {
        // Yes. Hang on to the result
        res = Ref(flags, artifact);

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
      auto newfile = env::createFile(c, flags.mode, false);

      // Link the new file into this directory
      auto link_version = addEntry(c, entry, newfile);

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
shared_ptr<DirVersion> DirArtifact::addEntry(const shared_ptr<Command>& c,
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

  // Inform the artifact of its new link
  target->addLinkUpdate(as<DirArtifact>(), entry, writing);

  // Add the new entry to the entries map
  _entries[entry] = {writing, target};

  // Notify the build of this output
  c->currentRun()->addContentOutput(shared_from_this(), writing);

  // Record this version in the artifact
  appendVersion(writing);

  return writing;
}

// Remove a directory entry from this artifact
shared_ptr<DirVersion> DirArtifact::removeEntry(const shared_ptr<Command>& c,
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
  c->currentRun()->addContentOutput(shared_from_this(), writing);

  // Record this version in the artifact as well
  appendVersion(writing);

  return writing;
}
