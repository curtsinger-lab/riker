#include "DirArtifact.hh"

#include <filesystem>
#include <memory>
#include <string>

#include "build/Env.hh"
#include "build/Resolution.hh"
#include "core/IR.hh"
#include "util/log.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

void DirArtifact::finalize(shared_ptr<Reference> ref) noexcept {
  // If we've been here before, don't finalize the directory again (symlinks can create cycles)
  if (_finalized) return;
  _finalized = true;

  // Coerce the reference to one that has a path
  auto a = dynamic_pointer_cast<Access>(ref);
  ASSERT(a) << "Somehow a directory was reached without a path";

  // Walk through and finalize each directory entry
  for (auto& [name, wp] : _entries) {
    auto artifact = wp.lock();
    if (name == "." || name == "..") continue;
    if (artifact) artifact->finalize(a->get(name, AccessFlags{}));
  }

  // Allow the artifact to finalize metadata
  Artifact::finalize(ref);
}

Resolution DirArtifact::getEntry(fs::path self, string entry) noexcept {
  if (entry == ".") return shared_from_this();
  if (entry == "..") return _env.getPath(self / entry);

  // If there is no record of this entry, look on the filesystem
  auto iter = _entries.find(entry);
  if (iter == _entries.end()) {
    // No match found. Ask the build for an artifact and save it in the map
    iter = _entries.emplace_hint(iter, entry, _env.getPath(self / entry));
  }

  auto artifact = iter->second.lock();

  if (artifact) {
    return artifact;
  } else {
    return ENOENT;
  }
}

void DirArtifact::setEntry(string entry, shared_ptr<Artifact> target) noexcept {
  _entries[entry] = target;
}
