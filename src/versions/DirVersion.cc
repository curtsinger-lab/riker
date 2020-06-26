#include "DirVersion.hh"

#include <memory>
#include <set>

#include <errno.h>
#include <unistd.h>

#include "build/Env.hh"
#include "core/AccessFlags.hh"
#include "core/IR.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::set;
using std::shared_ptr;

bool LinkVersion::canCommit() const noexcept {
  if (isCommitted()) return true;
  // We can only commit a link if its target can be committed
  return _target->getArtifact()->canCommit();
}

void LinkVersion::commit(fs::path path) noexcept {
  if (isCommitted()) return;

  // Just commit the reference that is linked. This will work in most cases, except when a build
  // creates a hard link from an existing artifact.
  if (_target->getArtifact()->isCommitted()) {
    INFO << "    already committed";
  } else {
    _target->getArtifact()->commit(path / _entry);
  }

  // Mark this version as committed
  Version::setCommitted();
}

void UnlinkVersion::commit(fs::path path) noexcept {
  if (isCommitted()) return;

  // Try to unlink the file
  int rc = ::unlink((path / _entry).c_str());

  // If the unlink failed because the target is a directory, try again with rmdir
  if (rc == -1 && errno == EISDIR) {
    rc = ::rmdir((path / _entry).c_str());
  }

  WARN_IF(rc != 0) << "Failed to unlink " << _entry << " from " << path << ": " << ERR;

  // Mark this version as committed
  Version::setCommitted();
}

void ExistingDirVersion::commit(fs::path path) noexcept {
  FAIL_IF(!isCommitted()) << "Existing directory versions can never be uncommitted";
}

void EmptyDirVersion::commit(fs::path path) noexcept {
  // TODO

  // Mark this version as committed
  Version::setCommitted();
}

/// Check if this version has a specific entry
optional<Resolution> ExistingDirVersion::getEntry(Env& env,
                                                  fs::path dir_path,
                                                  string name) noexcept {
  auto present_iter = _present.find(name);
  if (present_iter != _present.end()) return present_iter->second;

  auto absent_iter = _absent.find(name);
  if (absent_iter != _absent.end()) return ENOENT;

  // Check the environment for the file
  auto artifact = env.getPath(dir_path / name);
  if (artifact) {
    _present.emplace_hint(present_iter, name, artifact);
    return artifact;
  } else {
    _absent.emplace_hint(absent_iter, name);
    return ENOENT;
  }
}
