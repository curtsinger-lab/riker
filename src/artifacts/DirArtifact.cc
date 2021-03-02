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

DirArtifact::DirArtifact(shared_ptr<Command> c) noexcept : Artifact() {
  FAIL_IF(!c) << "A directory cannot be created by a null command";

  // Record the command that created this directory
  _creator = c;

  // Set up the base directory version
  auto base = make_shared<BaseDirVersion>(true);
  if (c->running()) {
    _committed_base_version = base;
  } else {
    _uncommitted_base_version = base;
  }

  // Add the base version to the creating command's outputs
  c->addDirectoryOutput(shared_from_this(), base);

  appendVersion(base);
}

DirArtifact::DirArtifact(shared_ptr<MetadataVersion> mv, shared_ptr<BaseDirVersion> dv) noexcept :
    Artifact(mv) {
  _committed_base_version = dv;
  appendVersion(dv);
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

  // Commit the versions needed for each entry
  for (auto& [entry, link] : _entries) {
    auto& [artifact, version, creator] = link;

    // Look for a corresponding committed entry
    auto iter = _committed_entries.find(entry);
    if (iter == _committed_entries.end()) {
      // If there is no matching committed entry, commit the link now
      artifact->commitLink(this->as<DirArtifact>(), entry);
      _committed_entries.emplace_hint(iter, entry, link);

    } else if (link != iter->second) {
      // If there is a committed entry with the same name but different contents, update it
      auto& [committed_artifact, committed_version, committed_creator] = iter->second;

      // Remove the link to the committed artifact (if there is one)
      if (committed_artifact) {
        committed_artifact->commitUnlink(this->as<DirArtifact>(), entry);
      }

      // Is there an artifact being linked? If so, link it.
      if (artifact) {
        artifact->commitLink(this->as<DirArtifact>(), entry);
      }

      // Update the map
      _committed_entries.emplace_hint(iter, entry, link);
    }
  }
}

/// Commit a link to this artifact at the given path
void DirArtifact::commitLink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  WARN << "Unimplemented DirArtifact::commitLink()";
}

/// Commit an unlink of this artifact at the given path
void DirArtifact::commitUnlink(std::shared_ptr<DirArtifact> dir, fs::path entry) noexcept {
  WARN << "Unimplemented DirArtifact::commitUnlink()";
}

// Compare all final versions of this artifact to the filesystem state
void DirArtifact::checkFinalState(fs::path path) noexcept {
  // Recursively check the final state of all known entries
  for (auto& [entry, link] : _entries) {
    auto& [artifact, version, creator] = link;

    // Do we expect the entry to point to an artifact?
    if (artifact) {
      // Yes. Make sure that artifact is in the expected final state
      artifact->checkFinalState(path / entry);
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
  for (auto& [entry, link] : _entries) {
    auto& [artifact, version, creator] = link;
    if (artifact) artifact->applyFinalState(path / entry);
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

  for (const auto& [entry, link] : _entries) {
    // Get the artifact, version, and creator for this entry
    const auto& [artifact, version, creator] = link;

    // If this entry is from the base version, we've already covered it
    if (version == base) continue;

    // Otherwise the entry is from some other version. Update the list.
    if (artifact) {
      result->addEntry(entry);
    } else {
      result->removeEntry(entry);
    }

    // The listing command depends on whatever version is responsible for this entry
    if (c) c->addDirectoryInput(shared_from_this(), version, creator, InputType::Accessed);
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
    // Get the version responsible for this entry and the artifact it mapped (possibly null)
    const auto& [artifact, version, creator] = entries_iter->second;

    // Is there an artifact to resolve to?
    if (artifact) {
      res = Ref(flags, artifact);
    } else {
      res = ENOENT;
    }

    // Add a path resolution input from the version that matched
    c->addDirectoryInput(shared_from_this(), version, creator, InputType::PathResolution);

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

        // Inform the artifact of its link in the current directory
        artifact->addLink(this->as<DirArtifact>(), entry);
        artifact->addCommittedLink(this->as<DirArtifact>(), entry);

      } else {
        // Set the result to ENOENT
        res = ENOENT;
      }

      // Add the entry to this directory's map of entries
      _entries.emplace(entry, Link{artifact, base, nullptr});

      // The entry is also committed
      _committed_entries.emplace(entry, Link{artifact, base, nullptr});
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
  // Create a partial version to track the committed state of this entry
  auto writing = make_shared<AddEntry>(entry, target);

  // Is there already an entry with this name?
  auto iter = _entries.find(entry);
  if (iter != _entries.end()) {
    auto& [old_artifact, old_version, old_creator] = iter->second;

    // Does the entry reference an artifact? It is losing its link
    if (old_artifact) {
      old_artifact->removeLink(this->as<DirArtifact>(), entry);
    }
  }

  // Add the entry to the map of all entries
  _entries[entry] = Link{target, writing, c};

  // Inform the artifact of its new link
  target->addLink(this->as<DirArtifact>(), entry);

  // Is the writing command running?
  if (c->running()) {
    // Add the link to the map of committed entries
    _committed_entries[entry] = Link{target, writing, c};

    // Inform the artifact of its new committed link
    target->addCommittedLink(this->as<DirArtifact>(), entry);
  }

  // Notify the build of this output
  c->addDirectoryOutput(shared_from_this(), writing);

  // Record this version in the artifact
  appendVersion(writing);
}

// Remove a directory entry from this artifact
void DirArtifact::removeEntry(const shared_ptr<Command>& c,
                              fs::path entry,
                              shared_ptr<Artifact> target) noexcept {
  // Create a partial version to track the committed state of this update
  auto writing = make_shared<RemoveEntry>(entry, target);

  // TODO: check for the old entry? We already have the artifact so maybe this isn't required

  // Update the map of entries
  _entries[entry] = Link{nullptr, writing, c};

  // Inform the target of its lost link
  target->removeLink(this->as<DirArtifact>(), entry);

  // Is the writing command running?
  if (c->running()) {
    // Remove the link from the map of committed entries too
    _committed_entries[entry] = Link{nullptr, writing, c};

    // Inform the artifact of its lost committed link
    target->removeCommittedLink(this->as<DirArtifact>(), entry);
  }

  // Notify the build of this output
  c->addDirectoryOutput(shared_from_this(), writing);

  // Record this version in the artifact as well
  appendVersion(writing);
}
