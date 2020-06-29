#include "DirArtifact.hh"

#include <list>
#include <memory>
#include <string>
#include <utility>

#include <dirent.h>
#include <sys/types.h>

#include "build/Build.hh"
#include "build/Env.hh"
#include "build/Resolution.hh"
#include "core/IR.hh"
#include "util/log.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tie;

DirArtifact::DirArtifact(Env& env,
                         shared_ptr<MetadataVersion> mv,
                         shared_ptr<DirVersion> dv) noexcept :
    Artifact(env, mv) {
  _dir_versions.push_front(dv);
  appendVersion(dv);
}

bool DirArtifact::canCommit(shared_ptr<Version> v) const noexcept {
  if (auto dv = v->as<DirVersion>()) {
    return dv->canCommit();
  } else {
    return Artifact::canCommit(v);
  }
}

void DirArtifact::commit(shared_ptr<Version> v) noexcept {
  if (auto dv = v->as<DirVersion>()) {
    dv->commit(this->as<DirArtifact>(), getPath());
  } else {
    Artifact::commit(v);
  }
}

bool DirArtifact::canCommitAll() const noexcept {
  // If this artifact's metadata cannot be committed, stop now
  if (!Artifact::canCommitAll()) return false;

  // Loop through versions. If any versions cannot be committed, return false;
  for (auto v : _dir_versions) {
    if (!v->canCommit()) return false;
  }

  // Everything is committable
  return true;
}

// Commit all final versions of this artifact to the filesystem
void DirArtifact::commitAll() noexcept {
  auto path = getPath();
  ASSERT(!path.empty()) << "Directory has no path";

  // Now walk through the versions in the order they were applied, and commit each one
  for (auto iter = _dir_versions.rbegin(); iter != _dir_versions.rend(); iter++) {
    auto v = *iter;
    v->commit(as<DirArtifact>(), path);
  }

  // Commit metadata through the Artifact base class
  Artifact::commitAll();
}

// Compare all final versions of this artifact to the filesystem state
void DirArtifact::checkFinalState() noexcept {
  // TODO: Check the final state of this directory against the filesystem
  // Linked entries should exist, and unlinked entries should not

  // Loop over all versions to build a full list of entries
  map<string, shared_ptr<Artifact>> entries;
  for (auto iter = _dir_versions.rbegin(); iter != _dir_versions.rend(); iter++) {
    auto v = *iter;
    v->getKnownEntries(entries);
  }

  // Now that we have known entries, recursively check the state of each
  for (auto [name, artifact] : entries) {
    artifact->checkFinalState();
  }

  // Check the metadata state as well
  Artifact::checkFinalState();
}

// Commit any pending versions and save fingerprints for this artifact
void DirArtifact::applyFinalState() noexcept {
  // First, commit this artifact and its metadata
  commitAll();

  // Fingerprint/commit any remaining metadata
  Artifact::applyFinalState();

  // Loop over all versions to build a full list of entries
  map<string, shared_ptr<Artifact>> entries;
  for (auto iter = _dir_versions.rbegin(); iter != _dir_versions.rend(); iter++) {
    auto v = *iter;
    v->getKnownEntries(entries);
  }

  // Now that we have known entries, recursively apply each one
  for (auto [name, artifact] : entries) {
    artifact->applyFinalState();
  }
}

Resolution DirArtifact::resolve(shared_ptr<Command> c,
                                shared_ptr<Artifact> prev,
                                fs::path::iterator current,
                                fs::path::iterator end,
                                shared_ptr<Access> ref,
                                bool committed) noexcept {
  // If the path has a trailing slash, the final entry will be empty. Advance past any empty entries
  while (current != end && current->empty()) current++;

  // If this is the last entry on the path, return this artifact
  if (current == end) return shared_from_this();

  // If the remaining path is not empty, make sure we have execute permission in this directory
  if (!checkAccess(c, AccessFlags{.x = true})) return EACCES;

  // We must be looking for an entry in this directory. Get the entry name and advance the iterator
  fs::path entry = *current++;

  // Are we looking for the current directory?
  if (entry == ".") return resolve(c, shared_from_this(), current, end, ref, committed);

  // Are we looking for the parent directory?
  if (entry == "..") {
    auto& links = getLinks();
    ASSERT(!links.empty()) << "Path resolution reached a directory with no parent";
    auto [parent, name] = *links.begin();
    return parent->resolve(c, shared_from_this(), current, end, ref, committed);
  }

  // Loop through versions to find one that refers to the requested entry
  Resolution result;
  for (auto& v : _dir_versions) {
    // Look for a matching entry in a version
    auto lookup = v->getEntry(_env, as<DirArtifact>(), entry);

    // Did the version give a definitive answer?
    if (lookup.has_value()) {
      // Yes. Save the result and break out of the loop
      result = lookup.value();

      // Commit the relevant version if requested
      if (committed) v->commit(as<DirArtifact>(), getPath());

      // TODO: Add a path resolution input from the version that matched

      break;
    }
  }

  // We now have either a resolved artifact or an error code. The next step depends on whether
  // this is the last part of the path, or if there is more path left to resolve.
  if (current == end) {
    // This is the last entry in the resolution path

    auto flags = ref->getFlags();

    // Was the reference required to create this entry?
    if (flags.create && flags.exclusive && result) return EEXIST;

    // If the resolution failed, can this access create it?
    if (flags.create && result == ENOENT) {
      // Can we write to this directory? If not, return an error
      if (!checkAccess(c, AccessFlags{.w = true})) return EACCES;

      // Create a new file
      auto newfile = _env.createFile(c, flags, committed);

      // Mark the final reference as resolved so we can link the file
      ref->resolvesTo(newfile);

      // Link the new file into this directory
      auto link_version = make_shared<LinkVersion>(entry, ref);
      link_version->createdBy(c);
      if (committed) link_version->setCommitted();
      apply(c, link_version);

      // The resolution result is now the newly-created file
      result = newfile;
    }

    // If the result was an error, return it
    if (!result) return result;

    // Update the resolved artifact's name
    result->addLink(as<DirArtifact>(), entry);

    // Otherwise continue with resolution, which may follow symlinks
    return result->resolve(c, shared_from_this(), current, end, ref, committed);

  } else {
    // There is still path left to resolve. Recursively resolve if the result succeeded
    if (result) {
      return result->resolve(c, shared_from_this(), current, end, ref, committed);
    }

    // Otherwise return the error from the resolution
    return result;
  }
}

// Apply a link version to this artifact
void DirArtifact::apply(shared_ptr<Command> c, shared_ptr<LinkVersion> writing) noexcept {
  // TODO: If this link is only possible because of some earlier version, add input edges.

  // Notify the build of this output
  _env.getBuild().observeOutput(c, shared_from_this(), writing);

  // Add the version to the sequence of directory versions
  _dir_versions.push_front(writing);

  // Record this version in the artifact as well
  appendVersion(writing);
}

// Apply an unlink version to this artifact
void DirArtifact::apply(shared_ptr<Command> c, shared_ptr<UnlinkVersion> writing) noexcept {
  // Walk through previous versions to see if there are any links to the same entry we are
  // unlinking
  for (auto& v : _dir_versions) {
    // Is v a link version?
    if (auto link = v->as<LinkVersion>()) {
      // Does the link refer to the same entry as the unlink?
      if (link->getEntryName() == writing->getEntryName()) {
        // Yes. If the previous link is uncommitted, we can mark the link-unlink pair as committed
        if (!link->isCommitted()) {
          link->setCommitted();
          writing->setCommitted();
        }
        // We found a match, so stop processing versions
        break;
      }
    }
  }

  // Notify the build of this output
  _env.getBuild().observeOutput(c, shared_from_this(), writing);

  // Add the version to the sequence of directory versions
  _dir_versions.push_front(writing);

  // Record this version in the artifact as well
  appendVersion(writing);
}
