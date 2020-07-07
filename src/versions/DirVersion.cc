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

bool AddEntry::canCommit() const noexcept {
  if (isCommitted()) return true;
  // We can only commit a link if its target can be committed
  return _target->getArtifact()->canCommitAll();
}

void AddEntry::commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept {
  if (isCommitted()) return;

  // Commit the link that this version applies
  _target->getArtifact()->commitLinkAt(dir, _entry);

  // Mark this version as committed
  Version::setCommitted();
}

bool RemoveEntry::canCommit() const noexcept {
  return true;
}

void RemoveEntry::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  if (isCommitted()) return;

  // Commit the unlink this version applies
  _target->getArtifact()->commitUnlinkAt(dir, _entry);

  // Mark this version as committed
  Version::setCommitted();
}

void ExistingDirVersion::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  FAIL_IF(!isCommitted()) << "Existing directory versions can never be uncommitted";
}

void EmptyDir::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  if (isCommitted()) return;

  int rc = ::mkdir(path.c_str(), 0755);
  WARN_IF(rc != 0) << "Failed to create directory " << path << ": " << ERR;

  // Mark this version as committed
  Version::setCommitted();
}

/// Check if this version has a specific entry
Resolution ExistingDirVersion::getEntry(Env& env,
                                        shared_ptr<DirArtifact> dir,
                                        string name) noexcept {
  // If we already know this entry is present, return it
  auto present_iter = _present.find(name);
  if (present_iter != _present.end()) return present_iter->second;

  // If we already know this entry is absent, return ENOENT
  auto absent_iter = _absent.find(name);
  if (absent_iter != _absent.end()) return ENOENT;

  // Create a path to the entry
  auto dir_path = dir->getCommittedPath();
  ASSERT(dir_path.has_value()) << "Directory has no path!";
  auto path = dir_path.value() / name;

  // This is a query for a new entry name. Try to stat the entry
  struct stat info;
  int rc = ::lstat(path.c_str(), &info);

  // If the lstat call failed, the entry does not exist
  if (rc != 0) {
    _absent.emplace_hint(absent_iter, name);
    return ENOENT;
  }

  // The artifact should exist. Get it from the environment and save it
  auto artifact = env.getArtifact(path, info);
  artifact->linkAt(dir, name, true);
  _present.emplace_hint(present_iter, name, artifact);
  return artifact;
}
