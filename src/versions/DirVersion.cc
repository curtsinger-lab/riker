#include "DirVersion.hh"

#include <memory>
#include <set>

#include <errno.h>
#include <unistd.h>

#include "build/Build.hh"
#include "build/Env.hh"
#include "core/AccessFlags.hh"
#include "core/IR.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::set;
using std::shared_ptr;

bool AddEntry::canCommit() const noexcept {
  // We can always commit a link to an artifact: it either has a path we can hard link to, or we
  // could create it.
  return true;
}

void AddEntry::commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept {
  if (isCommitted()) return;

  INFO << "Committing " << this;

  // Get the artifact we are linking
  auto artifact = _target->getArtifact();

  // First, check to see if the artifact has a temporary path
  auto temp_path = artifact->takeTemporaryPath();
  if (temp_path.has_value()) {
    // The artifact has a temporary path. We can move it to its new committed location
    LOG << "Moving " << artifact << " to " << dir_path / _entry;

    // Yes. Move the artifact into place
    int rc = ::rename(temp_path.value().c_str(), (dir_path / _entry).c_str());
    ASSERT(rc == 0) << "Failed to move " << artifact << " from a temporary location: " << ERR;

    // Mark this version as committed and return
    Version::setCommitted();
    return;
  }

  // The artifact does not have a temporary path. Get all the links to this artifact
  auto [committed_links, uncommitted_links] = artifact->getLinks();

  // Does the artifact have at least one committed link?
  if (committed_links.size() > 0) {
    // Pull apart one of the committed links
    auto [old_link, old_version] = *committed_links.begin();
    auto [old_dir, old_entry] = old_link;

    // Get the path to the existing link's directory
    auto old_dir_path = old_dir->getCommittedPath();
    ASSERT(old_dir_path.has_value()) << "Link was committed to a directory with no path";
    auto existing_path = old_dir_path.value() / old_entry;

    // There is at least one committed link. If the artifact is a directory, we need to move it. If
    // not, we can hard link to it.
    if (auto artifact_dir = artifact->as<DirArtifact>()) {
      // The artifact is a directory
      FAIL << "Moving directories is not supported yet";

      return;

    } else {
      // The artifact is a file, so we can create a hard link to it

      // Record a dependency on the version that created the hard link
      // TODO: We could depend on any of the existing hard links, not just the first one.
      dir->getEnv().getBuild().observeInput(getCreator(), artifact, old_version,
                                            InputType::Accessed);

      // Make the hard link
      int rc = ::link(existing_path.c_str(), (dir_path / _entry).c_str());
      if (rc != 0) {
        auto [committed_links, uncommitted_links] = artifact->getLinks();
        INFO << "Committed Links:";
        for (auto [link, version] : committed_links) {
          auto [link_dir, link_name] = link;
          INFO << "  " << link_dir->getCommittedPath().value() / link_name << " (" << version
               << ")";
          INFO << "  version is " << (version->isCommitted() ? "committed" : "uncommitted");
        }
        INFO << "Uncommitted Links:";
        for (auto [link, version] : uncommitted_links) {
          auto [link_dir, link_name] = link;
          INFO << "  " << link_dir->getPath().value() / link_name << " (" << version << ")";
        }
      }

      ASSERT(rc == 0) << "Failed to hard link " << artifact << " to " << dir_path / _entry << ": "
                      << ERR;

      // Mark this version as committed and return
      Version::setCommitted();
      return;
    }

  } else {
    // The artifact has no committed links. Committing the artifact *should* create it (we probably
    // need to check this).

    // Mark this version as committed so the artifact can use it as a committed path
    Version::setCommitted();

    // Now commit the artifact
    artifact->commitAll();
  }
}

bool RemoveEntry::canCommit() const noexcept {
  return true;
}

void RemoveEntry::commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept {
  if (isCommitted()) return;

  INFO << "Committing " << this;

  // Get the artifact we're unlinking
  auto artifact = _target->getArtifact();

  // We need to know all the links to this artifact
  auto [committed_links, uncommitted_links] = artifact->getLinks();

  // If this artifact has uncommitted links but is losing its last committed link...
  if (committed_links.size() == 1 && uncommitted_links.size() > 0) {
    // We need to preserve the artifact. We'll move it to a temporary location.
    auto temp_path = artifact->assignTemporaryPath();

    // Move the artifact
    int rc = ::rename((dir_path / _entry).c_str(), temp_path.c_str());
    ASSERT(rc == 0) << "Failed to move " << artifact << " to a temporary location: " << ERR;

    // Mark this version as committed and return
    Version::setCommitted();
    return;
  }

  // The artifact is either losing its last link (committed and uncommitted) or has remaining links
  // We need to unlink or rmdir it, depending on the type of artifact
  if (auto artifact_dir = artifact->as<DirArtifact>()) {
    // The artifact is a directory. We will call rmdir, but first we need to commit any pending
    // versions that will remove the directory's entries
    artifact_dir->commitAll();
    int rc = ::rmdir((dir_path / _entry).c_str());
    ASSERT(rc == 0) << "Failed to remove directory " << artifact_dir << ": " << ERR;

    // Mark this version as committed and return
    Version::setCommitted();
    return;

  } else {
    // The artifact is a file, symlink etc. that can be hard linked. Just unlink it.
    int rc = ::unlink((dir_path / _entry).c_str());
    ASSERT(rc == 0) << "Failed to unlink " << artifact << " from " << dir_path / _entry << ": "
                    << ERR;

    // Mark this version as committed and return
    Version::setCommitted();
    return;
  }
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
  artifact->addLinkUpdate(dir, name, this->as<ExistingDirVersion>());
  _present.emplace_hint(present_iter, name, artifact);
  return artifact;
}
