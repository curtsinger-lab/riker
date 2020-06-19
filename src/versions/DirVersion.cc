#include "DirVersion.hh"

#include <memory>
#include <set>

#include "build/Env.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::set;
using std::shared_ptr;

/// Check if this version has a specific entry
Lookup ExistingDirVersion::hasEntry(Env& env, shared_ptr<Access> ref, string name) noexcept {
  auto present_iter = _present.find(name);
  if (present_iter != _present.end()) return Lookup::Yes;

  auto absent_iter = _absent.find(name);
  if (absent_iter != _absent.end()) return Lookup::No;

  // Check the environment for the file
  auto artifact = env.getPath(ref->getFullPath() / name);
  if (artifact) {
    _present.emplace_hint(present_iter, name);
    return Lookup::Yes;
  } else {
    _absent.emplace_hint(absent_iter, name);
    return Lookup::No;
  }
}
