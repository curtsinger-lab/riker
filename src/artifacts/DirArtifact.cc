#include "DirArtifact.hh"

#include <filesystem>
#include <list>
#include <memory>
#include <string>
#include <utility>

#include <dirent.h>
#include <sys/types.h>

#include "build/Env.hh"
#include "build/Resolution.hh"
#include "core/IR.hh"
#include "util/log.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;
using std::tie;

namespace fs = std::filesystem;

DirArtifact::DirArtifact(Env& env,
                         bool committed,
                         shared_ptr<MetadataVersion> mv,
                         shared_ptr<DirVersion> dv) noexcept :
    Artifact(env, committed, mv) {
  appendVersion(dv);
  _dir_versions.push_front(dv);
  _dir_committed = committed;
}

void DirArtifact::finalize(shared_ptr<Reference> ref) noexcept {
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
    if (artifact) artifact->finalize(a->get(name, AccessFlags{}));
  }

  // Allow the artifact to finalize metadata
  Artifact::finalize(ref);
}

Resolution DirArtifact::getEntry(fs::path self, fs::path entry) noexcept {
  // Look in the cache of resolved artifacts
  auto iter = _resolved.find(entry);
  if (iter != _resolved.end()) {
    return iter->second.lock();
  }

  // If we're looking for ., cache it and return
  if (entry == ".") {
    _resolved.emplace(".", shared_from_this());
    return shared_from_this();
  }

  // If we find an entry, the artifact will go here
  shared_ptr<Artifact> artifact;

  // Loop through versions until we get a definite answer about the entry
  Lookup found;
  for (auto& v : _dir_versions) {
    // Ask the specific version
    found = v->hasEntry(_env, self, entry);

    // Handle the result
    if (found == Lookup::No) {
      // No entry, so return an error
      return ENOENT;
    } else if (found == Lookup::Yes) {
      // Ask the version to provide the artifact if it can
      artifact = v->getEntry(entry);

      // Found the entry, so break out of the loop
      break;
    }
  }

  // This should never happen...
  ASSERT(found != Lookup::Maybe) << "Directory lookup concluded without a definite answer";

  // If the artifact is null at this point, we're supposed to look on the filesystem
  if (!artifact) {
    artifact = _env.getPath(self / entry);
    ASSERT(artifact) << "Failed to locate artifact for existing entry at " << (self / entry);
  }

  // Cache the resolved artifact
  _resolved.emplace_hint(iter, entry, artifact);

  return artifact;
}

void DirArtifact::setEntry(fs::path entry, shared_ptr<Artifact> target) noexcept {
  _resolved[entry] = target;
}
