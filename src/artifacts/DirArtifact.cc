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

bool DirArtifact::canCommit() const noexcept {
  // Loop through versions. If any versions cannot be committed, return false;
  for (auto v : _dir_versions) {
    if (!v->canCommit()) return false;
  }

  // Otherwise, just check with the artifact to see if we can commit metadata
  return Artifact::canCommit();
}

bool DirArtifact::isCommitted() const noexcept {
  for (auto v : _dir_versions) {
    if (!v->isCommitted()) return false;
  }
  return Artifact::isCommitted();
}

void DirArtifact::commit(shared_ptr<Reference> ref) noexcept {
  map<string, shared_ptr<LinkVersion>> links;
  map<string, shared_ptr<UnlinkVersion>> unlinks;

  // Walk through all versions of this directory in the order they were applied
  // We're looking for pairs of versions that can be canceled out:
  //  1. An link followed by an unlink: both can be skipped
  //  2. An unlink followed by a link to an already-committed artifact: both can be skipped
  for (auto iter = _dir_versions.rbegin(); iter != _dir_versions.rend(); iter++) {
    auto v = *iter;

    if (v->isCommitted()) continue;

    // What kind of version is this?
    if (auto link = v->as<LinkVersion>()) {
      // If this link points to a committed artifact, wipe out any earlier unlinks of this entry
      if (link->getTarget()->getArtifact()->isCommitted()) {
        auto unlink_iter = unlinks.find(link->getEntryName());
        if (unlink_iter != unlinks.end()) {
          unlink_iter->second->setCommitted();
          link->setCommitted();
        }

      } else {
        links.emplace(link->getEntryName(), link);
      }

    } else if (auto unlink = v->as<UnlinkVersion>()) {
      // Skip over committed unlinks
      if (unlink->isCommitted()) continue;

      // Is there an earlier link this undoes?
      auto link_iter = links.find(unlink->getEntryName());
      if (link_iter != links.end()) {
        // Found a pair. Mark both versions as committed
        link_iter->second->setCommitted();
        unlink->setCommitted();

        // Remove the link entry from the map
        links.erase(link_iter);
      } else {
        // Remember this uncommitted unlink
        unlinks.emplace(unlink->getEntryName(), unlink);
      }
    }
  }

  // Now walk through the versions again and commit them. Any canceled-out versions are already
  // committed.
  for (auto iter = _dir_versions.rbegin(); iter != _dir_versions.rend(); iter++) {
    auto v = *iter;
    WARN_IF(!v->isCommitted()) << this << ": committing " << v;
    v->commit(ref);
  }

  // Commit metadata through the Artifact base class
  Artifact::commit(ref);
}

void DirArtifact::finalize(shared_ptr<Reference> ref, bool commit) noexcept {
  // If we've been here before, don't finalize the directory again (symlinks can create cycles)
  if (_finalized) return;
  _finalized = true;

  // Coerce the reference to one that has a path
  auto a = ref->as<Access>();
  ASSERT(a) << "Somehow a directory was reached without a path";

  // Walk through and finalize each directory entry
  for (auto& [name, wp] : _resolved) {
    auto artifact = wp.lock();
    if (name == "." || name == "..") continue;
    if (artifact) artifact->finalize(make_shared<Access>(a, name, AccessFlags{}), commit);
  }

  // Allow the artifact to finalize metadata
  Artifact::finalize(ref, commit);
}

Resolution DirArtifact::getEntry(shared_ptr<Command> c,
                                 shared_ptr<Reference> ref,
                                 string entry) noexcept {
  // If we're looking for ".", return immediately
  if (entry == ".") return shared_from_this();

  auto access = ref->as<Access>();
  ASSERT(access) << "Program somehow reached a directory without a path";

  // Loop through versions until we get a definite answer about the entry
  Lookup found = Lookup::Maybe;
  shared_ptr<DirVersion> matched;

  // First check the uncommitted versions
  for (auto& v : _dir_versions) {
    found = v->hasEntry(_env, access, entry);
    if (found != Lookup::Maybe) {
      matched = v;
      break;
    }
  }

  // Make sure we have a definite result
  ASSERT(found != Lookup::Maybe) << "Directory lookup concluded without a definite answer";

  if (found == Lookup::Yes) {
    // Record the dependency on the matching version
    _env.getBuild().observeInput(c, ref, shared_from_this(), matched, InputType::PathResolution);

    // Look in the cache of resolved artifacts
    auto iter = _resolved.find(entry);
    if (iter != _resolved.end()) {
      // Found a match. Return it now.
      return iter->second.lock();
    } else {
      // No cached artifact. Ask the version to provide the artifact if it can
      auto artifact = matched->getEntry(entry);

      // If the version did not provide an artifact, look in the environment
      if (!artifact) {
        artifact = _env.getPath(access->getFullPath() / entry);
        ASSERT(artifact) << "Failed to locate artifact for existing entry " << entry << " in "
                         << ref;
      }

      // Save the resolved artifact in the cache and return
      _resolved.emplace_hint(iter, entry, artifact);
      return artifact;
    }

  } else {
    // The entry does not exist. Record a dependency on the version that excluded this entry.
    _env.getBuild().observeInput(c, ref, shared_from_this(), matched, InputType::PathResolution);
    return ENOENT;
  }
}

// Apply a link version to this artifact
void DirArtifact::apply(shared_ptr<Command> c,
                        shared_ptr<Reference> ref,
                        shared_ptr<LinkVersion> writing) noexcept {
  // TODO: If this link is only possible because of some earlier version, add input edges.

  // Notify the build of this output
  _env.getBuild().observeOutput(c, shared_from_this(), writing);

  // Add the version to the sequence of directory versions
  _dir_versions.push_front(writing);

  // Record this version in the artifact as well
  appendVersion(writing);

  // Cache the resolution for this linked artifact
  _resolved[writing->getEntryName()] = writing->getTarget()->getArtifact();
}

// Apply an unlink version to this artifact
void DirArtifact::apply(shared_ptr<Command> c,
                        shared_ptr<Reference> ref,
                        shared_ptr<UnlinkVersion> writing) noexcept {
  // TODO: If this unlink is only possible because of some earlier version, add input edges.

  // Notify the build of this output
  _env.getBuild().observeOutput(c, shared_from_this(), writing);

  // Add the version to the sequence of directory versions
  _dir_versions.push_front(writing);

  // Record this version in the artifact as well
  appendVersion(writing);

  // Remove the unlinked entry from the cache of resolved artifacts
  _resolved.erase(writing->getEntryName());
}
