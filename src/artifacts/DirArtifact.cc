#include "DirArtifact.hh"

#include <filesystem>
#include <list>
#include <memory>
#include <string>

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

  // Loop through versions until we get a definite answer about the entry
  Lookup result;
  for (auto& v : _dir_versions) {
    // Ask the specific version
    result = v->hasEntry(entry);

    // Handle the result
    if (result == Lookup::No) {
      // No entry, so return an error
      return ENOENT;
    } else if (result == Lookup::Yes) {
      // Found the entry, so break out of the loop
      break;
    }
  }

  // This should never happen...
  ASSERT(result != Lookup::Maybe) << "Directory lookup concluded without a definite answer";

  // The entry exists, so now try to resolve it
  auto artifact = _env.getPath(self / entry);

  // Cache the resolved artifact
  _resolved.emplace_hint(iter, entry, artifact);

  return artifact;
}

void DirArtifact::setEntry(fs::path entry, shared_ptr<Artifact> target) noexcept {
  _resolved[entry] = target;
}
