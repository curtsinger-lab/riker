#include "DirVersion.hh"

#include <memory>
#include <set>

#include <unistd.h>

#include "build/Env.hh"
#include "core/AccessFlags.hh"
#include "core/IR.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::set;
using std::shared_ptr;

void LinkVersion::commit(shared_ptr<Reference> dir_ref) noexcept {
  // Just commit the reference that is linked. This will work in most cases, except when a build
  // creates a hard link from an existing artifact.
  auto access = dir_ref->as<Access>();
  ASSERT(access) << "Tried to commit a directory with a non-path reference";

  auto entry_ref = make_shared<Access>(access, _entry, AccessFlags{});
  if (_target->getArtifact()->isCommitted()) {
    INFO << "    already committed";
  } else {
    _target->getArtifact()->commit(entry_ref);
  }
}

void UnlinkVersion::commit(shared_ptr<Reference> dir_ref) noexcept {
  auto access = dir_ref->as<Access>();
  ASSERT(access) << "Tried to commit a directory with a non-path reference";

  int rc = ::unlink((access->getFullPath() / _entry).c_str());
  WARN_IF(rc != 0) << "Failed to unlink " << _entry << " from " << dir_ref;

  // TODO: handle unlinks that are actually rmdir operations
}

void ExistingDirVersion::commit(shared_ptr<Reference> dir_ref) noexcept {
  FAIL << "Tried to commit an existing directory: " << dir_ref;
}

void EmptyDirVersion::commit(shared_ptr<Reference> dir_ref) noexcept {
  // TODO
}

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
