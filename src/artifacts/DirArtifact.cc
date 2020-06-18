#include "DirArtifact.hh"

#include <filesystem>
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

Resolution DirArtifact::getEntry(shared_ptr<Command> c, fs::path dirpath, fs::path entry) noexcept {
  // If we're looking for ".", return immediately
  if (entry == ".") return shared_from_this();

  // Loop through versions until we get a definite answer about the entry
  Lookup found;
  for (auto& v : _dir_versions) {
    // Ask the specific version
    found = v->hasEntry(_env, dirpath, entry);

    // Handle the result
    if (found == Lookup::No) {
      // No entry. Record the dependency and return an error
      _env.getBuild().observeInput(c, shared_from_this(), v, InputType::PathResolution);
      return ENOENT;

    } else if (found == Lookup::Yes) {
      // Found an entry. Record the dependency
      _env.getBuild().observeInput(c, shared_from_this(), v, InputType::PathResolution);

      // Look in the cache of resolved artifacts
      auto iter = _resolved.find(entry);
      if (iter != _resolved.end()) {
        // Found a match. Return it now.
        return iter->second.lock();
      } else {
        // Ask the version to provide the artifact if it can
        auto artifact = v->getEntry(entry);

        // If the version did not provide an artifact, look in the environment
        if (!artifact) {
          artifact = _env.getPath(dirpath / entry);
          ASSERT(artifact) << "Failed to locate artifact for existing entry at "
                           << (dirpath / entry);
        }

        // Save the resolved artifact in the cache and return
        _resolved.emplace_hint(iter, entry, artifact);
        return artifact;
      }
    }
  }

  // This should never happen...
  FAIL_IF(found == Lookup::Maybe) << "Directory lookup concluded without a definite answer";
  return ENOENT;
}

void DirArtifact::addEntry(shared_ptr<Command> c,
                           fs::path dirpath,
                           fs::path entry,
                           shared_ptr<Reference> target) noexcept {
  // Create a new version to encode the linking operation
  auto v = make_shared<LinkDirVersion>(entry, target);
  v->createdBy(c);

  // Notify the build of this output
  _env.getBuild().observeOutput(c, shared_from_this(), v);

  // Add this to the front of the directory version list
  _dir_versions.push_front(v);

  // Record this version in the artifact as well
  appendVersion(v);

  // Cache the result of the resolution
  _resolved[entry] = target->getArtifact();
}
