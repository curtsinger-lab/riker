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

void LinkVersion::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  if (isCommitted()) return;

  // Inform the target of this new link
  _target->getArtifact()->addLink(dir, _entry);

  // Just commit the reference that is linked. This will work in most cases, except when a build
  // creates a hard link from an existing artifact.
  if (_target->getArtifact()->isCommitted()) {
    INFO << "    already committed";
  } else {
    _target->getArtifact()->commit();
  }

  // Mark this version as committed
  Version::setCommitted();
}

void UnlinkVersion::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  if (isCommitted()) return;

  // TODO: Remove this link from the artifact we've unlinked

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

void ExistingDirVersion::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  FAIL_IF(!isCommitted()) << "Existing directory versions can never be uncommitted";
}

void EmptyDirVersion::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  // TODO

  // Mark this version as committed
  Version::setCommitted();
}

/// Check if this version has a specific entry
optional<Resolution> ExistingDirVersion::getEntry(Env& env,
                                                  fs::path dir_path,
                                                  shared_ptr<DirArtifact> dir,
                                                  string name) noexcept {
  // If we already know this entry is present, return it
  auto present_iter = _present.find(name);
  if (present_iter != _present.end()) return present_iter->second;

  // If we already know this entry is absent, return ENOENT
  auto absent_iter = _absent.find(name);
  if (absent_iter != _absent.end()) return ENOENT;

  // This is a query for a new entry name. Try to stat the entry
  struct stat info;
  int rc = ::lstat((dir_path / name).c_str(), &info);

  // If the lstat call failed, the entry does not exist
  if (rc != 0) {
    _absent.emplace_hint(absent_iter, name);
    return ENOENT;
  }

  // The artifact should exist. Get it from the environment and save it
  auto artifact = env.getArtifact(dir_path / name, info);
  artifact->addLink(dir, name);
  _present.emplace_hint(present_iter, name, artifact);
  return artifact;
}
