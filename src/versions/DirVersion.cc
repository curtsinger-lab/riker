#include "DirVersion.hh"

#include <filesystem>
#include <memory>

#include <errno.h>
#include <unistd.h>

#include "build/Build.hh"
#include "build/Env.hh"
#include "core/AccessFlags.hh"
#include "core/IR.hh"
#include "util/serializer.hh"
#include "versions/Version.hh"

using std::shared_ptr;

namespace fs = std::filesystem;

// Commit a directory creation
void CreatedDir::commit(shared_ptr<DirArtifact> dir, fs::path path) noexcept {
  if (isCommitted()) return;

  int rc = ::mkdir(path.c_str(), 0755);
  ASSERT(rc == 0) << "Failed to create directory " << path << ": " << ERR;

  // Mark this version as committed
  Version::setCommitted();
}

// Check if an existing directory has a specific entry
Resolution ExistingDir::getEntry(Build& build,
                                 shared_ptr<Env> env,
                                 shared_ptr<DirArtifact> dir,
                                 string name) noexcept {
  // Create a path to the entry. Start with a committed path to the directory
  auto dir_path = dir->getPath(false);
  ASSERT(dir_path.has_value()) << "Directory has no path!";
  auto path = dir_path.value() / name;

  // Try to get the artifact from the filesystem
  auto artifact = env->getFilesystemArtifact(path);

  // If no artifact was returned, it must not exist
  if (!artifact) return ENOENT;

  // We found an artifact, so record its presence
  artifact->addLinkUpdate(dir, name, this->as<ExistingDir>());
  return artifact;
}

// Create a listed directory version from an existing directory
shared_ptr<ListedDir> ExistingDir::getList(shared_ptr<Env> env, shared_ptr<DirArtifact> dir) const
    noexcept {
  auto result = make_shared<ListedDir>();

  // Get a path to the directory, but only allow committed paths
  auto path = dir->getPath(false);
  ASSERT(path.has_value()) << "Existing directory somehow has no committed path";

  for (auto& entry : fs::directory_iterator(path.value())) {
    auto name = entry.path().stem();
    if (name != ".dodo") {
      result->addEntry(entry.path().stem());
    }
  }

  return result;
}

// Apply an AddEntry version to an artifact
void AddEntry::applyTo(Build& b, shared_ptr<Command> c, shared_ptr<Artifact> a) noexcept {
  a->applyContent(b, c, this->as<AddEntry>());
}

// Commit the addition of an entry to a directory
void AddEntry::commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept {
  if (isCommitted()) return;

  // Get the artifact we are linking
  auto artifact = _target->getArtifact();

  // First, check to see if the artifact has a temporary path
  auto temp_path = artifact->takeTemporaryPath();
  if (temp_path.has_value()) {
    // The artifact has a temporary path. We can move it to its new committed location
    LOG(artifact) << "Moving " << artifact << " from temporary location to " << dir_path / _entry;

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
    auto old_dir_path = old_dir->getPath(false);
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

      auto new_path = dir_path / _entry;

      // Make the hard link
      int rc = ::link(existing_path.c_str(), new_path.c_str());
      if (rc && errno == EEXIST) {
        rc = ::unlink(new_path.c_str());
        ASSERT(rc == 0) << "Failed to remove existing link at " << new_path;
        rc = ::link(existing_path.c_str(), new_path.c_str());
      }
      ASSERT(rc == 0) << "Failed to hard link " << artifact << " to " << dir_path / _entry << ": "
                      << ERR;

      // Mark this version as committed and return
      Version::setCommitted();
      return;
    }
  } else {
    // The artifact has no committed links. Committing the artifact *should* create it (we
    // probably need to check this).

    // Mark this version as committed so the artifact can use it as a committed path
    Version::setCommitted();

    // Now commit the artifact
    artifact->commitAll();
  }
}

// Apply an RemoveEntry version to an artifact
void RemoveEntry::applyTo(Build& b, shared_ptr<Command> c, shared_ptr<Artifact> a) noexcept {
  a->applyContent(b, c, this->as<RemoveEntry>());
}

// Commit the removal of an entry from a directory
void RemoveEntry::commit(shared_ptr<DirArtifact> dir, fs::path dir_path) noexcept {
  if (isCommitted()) return;

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

  // The artifact is either losing its last link (committed and uncommitted) or has remaining
  // links We need to unlink or rmdir it, depending on the type of artifact
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
