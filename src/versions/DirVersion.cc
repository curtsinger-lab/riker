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

  // Try to get a path for the target artifact
  auto target_path = _target->getArtifact()->getPath();

  // Does the target have a path?
  if (!target_path.empty()) {
    // Yes. Create a hard link to the target
    ::link(target_path.c_str(), (dir_path / _entry).c_str());

    // Inform the target artifact of its new link
    _target->getArtifact()->addLink(dir, _entry);

  } else {
    // No. Add a link to the target artifact, then commit it
    _target->getArtifact()->addLink(dir, _entry);
    _target->getArtifact()->commitAll();
  }

  // Mark this version as committed
  Version::setCommitted();
}

void RemoveEntry::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  if (isCommitted()) return;

  // Is the target artifact a directory?
  if (auto target_dir = _target->getArtifact()->as<DirArtifact>()) {
    // Before we can remove the directory, we need to commit any of its remaining versions
    target_dir->commitAll();

    // Remove the directory
    int rc = ::rmdir((path / _entry).c_str());
    WARN_IF(rc != 0) << "Failed to remove directory " << _entry << " from " << path << ": " << ERR;

  } else {
    // Unlink the file
    int rc = ::unlink((path / _entry).c_str());
    WARN_IF(rc != 0) << "Failed to unlink " << _entry << " from " << path << ": " << ERR;
  }

  // Remove the link to the artifact
  _target->getArtifact()->removeLink(dir, _entry);

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
  auto path = dir->getPath() / name;

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
  artifact->addLink(dir, name);
  _present.emplace_hint(present_iter, name, artifact);
  return artifact;
}
