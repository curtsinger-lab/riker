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
  // If this artifact's metadata cannot be committed, stop now
  if (!Artifact::canCommit()) return false;

  // Loop through versions. If any versions cannot be committed, return false;
  for (auto v : _dir_versions) {
    if (!v->canCommit()) return false;
  }

  // Everything is committable
  return true;
}

bool DirArtifact::isCommitted() const noexcept {
  for (auto v : _dir_versions) {
    if (!v->isCommitted()) return false;
  }
  return Artifact::isCommitted();
}

void DirArtifact::commit(shared_ptr<Reference> ref) noexcept {
  // Now walk through the versions in the order they were applied, and commit each one
  for (auto iter = _dir_versions.rbegin(); iter != _dir_versions.rend(); iter++) {
    auto v = *iter;
    v->commit(ref);
  }

  // Commit metadata through the Artifact base class
  Artifact::commit(ref);
}

void DirArtifact::finalize(shared_ptr<Reference> ref, bool commit) noexcept {
  // If we've been here before, don't finalize the directory again (symlinks can create cycles)
  if (_finalized) return;
  _finalized = true;

  auto access = ref->as<Access>();

  // Loop over all versions to build a full list of entries
  map<string, shared_ptr<Artifact>> entries;
  for (auto& v : _dir_versions) {
    v->getKnownEntries(entries);
  }

  for (auto [name, artifact] : entries) {
    artifact->finalize(make_shared<Access>(access, name, AccessFlags{}), commit);
  }

  // Allow the artifact to finalize metadata
  Artifact::finalize(ref, commit);
}

Resolution DirArtifact::getEntry(shared_ptr<Command> c,
                                 shared_ptr<Reference> ref,
                                 string entry) noexcept {
  auto access = ref->as<Access>();
  ASSERT(access) << "Program somehow reached a directory without a path";

  fs::path dir_path = access->getFullPath();

  // If we're looking for entry ".", return this directory
  if (entry == ".") return shared_from_this();

  // If we're looking for entry "..", get the parent directory
  if (entry == "..") return _env.getPath(dir_path.parent_path());

  // Loop through versions until we get a definite answer about the entry
  for (auto& v : _dir_versions) {
    auto result = v->getEntry(_env, dir_path, entry);
    // If the version returned something other than nullopt, that's the result
    if (result.has_value()) {
      _env.getBuild().observeInput(c, ref, shared_from_this(), v, InputType::PathResolution);
      return result.value();
    }
  }

  // We should never hit this case becuase directories should always have a complete version.
  FAIL << "Directory access ran out of versions.";
  return ENOENT;
}

Resolution DirArtifact::resolve(shared_ptr<Command> c,
                                shared_ptr<DirArtifact> parent,
                                fs::path resolved,
                                fs::path remaining,
                                AccessFlags flags) noexcept {
  // If this is the last entry on the path, return this artifact
  if (remaining.empty()) return shared_from_this();

  // We must be looking for an entry in this directory
  // Split the remaining path into the entry in this directory, and the rest of the remaining path
  auto iter = remaining.begin();
  fs::path entry = *iter++;
  fs::path rest;
  while (iter != remaining.end()) rest /= *iter++;

  // Loop through versions to find one that refers to the requested entry
  // for (auto& v : _dir_versions) {
  //  auto found = v->hasEntry
  //}
  return ENOENT;
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
}

// Apply an unlink version to this artifact
void DirArtifact::apply(shared_ptr<Command> c,
                        shared_ptr<Reference> ref,
                        shared_ptr<UnlinkVersion> writing) noexcept {
  // Walk through previous versions to see if there are any links to the same entry we are unlinking
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
