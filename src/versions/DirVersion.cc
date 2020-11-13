#include "DirVersion.hh"

#include <filesystem>
#include <memory>

#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifacts/DirArtifact.hh"
#include "data/AccessFlags.hh"
#include "runtime/Build.hh"
#include "runtime/Env.hh"
#include "util/serializer.hh"
#include "versions/DirListVersion.hh"
#include "versions/Version.hh"

using std::shared_ptr;

namespace fs = std::filesystem;

// Commit a base directory version
void BaseDirVersion::commit(fs::path path) noexcept {
  if (isCommitted()) return;

  // This directory better be one we've created, otherwise it should have been committed already
  ASSERT(_created) << "An on-disk directory is somehow not committed";

  struct stat statbuf;

  // does the directory already exist?
  int rc = ::lstat(path.c_str(), &statbuf);
  if (rc == -1) {
    if (errno == ENOENT) {
      // dir doesn't exist-- go ahead and create it
      rc = ::mkdir(path.c_str(), 0755);

      FAIL_IF(rc != 0) << "Failed to create directory " << path << ": " << ERR;
    } else {
      // some error we're not expecting
      FAIL_IF(rc != 0) << "Unable to stat " << path << " (" << strerror(errno) << ")";
    }
  } else {  // it exists
    // decode
    mode_flags flags(statbuf.st_mode);

    // file exists-- but is it a directory?
    FAIL_IF(!flags.isDirectory()) << "File at " << path
                                  << " exists but is not a directory as expected.  File is "
                                  << flags.filetype_str() << " (" << (statbuf.st_mode & S_IFMT)
                                  << ").";
  }
  
  // Mark this version as committed
  Version::setCommitted();
}

// Commit the addition of an entry to a directory
void AddEntry::commit(fs::path dir_path) noexcept {
  if (isCommitted()) return;

  // First, check to see if the linked artifact has a temporary path
  auto temp_path = _target->takeTemporaryPath();
  if (temp_path.has_value()) {
    // The artifact has a temporary path. We can move it to its new committed location
    LOG(artifact) << "Moving " << _target << " from temporary location to " << dir_path / _entry;

    // Yes. Move the artifact into place
    int rc = ::rename(temp_path.value().c_str(), (dir_path / _entry).c_str());
    FAIL_IF(rc != 0) << "Failed to move " << _target << " from a temporary location: " << ERR;

    // Mark this version as committed and return
    Version::setCommitted();
    return;
  }

  // The artifact does not have a temporary path. Get all the links to this artifact
  auto [committed_links, uncommitted_links] = _target->getLinks();

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
    if (auto artifact_dir = _target->as<DirArtifact>()) {
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
      ASSERT(rc == 0) << "Failed to hard link " << _target << " to " << dir_path / _entry << ": "
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
    _target->commitAll();
  }
}

// Commit the removal of an entry from a directory
void RemoveEntry::commit(fs::path dir_path) noexcept {
  if (isCommitted()) return;

  // We need to know all the links to the artifact being unlinked
  auto [committed_links, uncommitted_links] = _target->getLinks();

  // If this artifact has uncommitted links but is losing its last committed link...
  if (committed_links.size() == 1 && uncommitted_links.size() > 0) {
    // We need to preserve the artifact. We'll move it to a temporary location.
    auto temp_path = _target->assignTemporaryPath();

    // Move the artifact
    int rc = ::rename((dir_path / _entry).c_str(), temp_path.c_str());
    FAIL_IF(rc != 0) << "Failed to move " << _target << " to a temporary location: " << ERR;

    // Mark this version as committed and return
    Version::setCommitted();
    return;
  }

  // The artifact is either losing its last link (committed and uncommitted) or has remaining
  // links We need to unlink or rmdir it, depending on the type of artifact
  if (auto artifact_dir = _target->as<DirArtifact>()) {
    // The artifact is a directory. We will call rmdir, but first we need to commit any pending
    // versions that will remove the directory's entries
    artifact_dir->commitAll();
    int rc = ::rmdir((dir_path / _entry).c_str());
    FAIL_IF(rc != 0) << "Failed to remove directory " << artifact_dir << ": " << ERR;

    // Mark this version as committed and return
    Version::setCommitted();
    return;

  } else {
    // The artifact is a file, symlink etc. that can be hard linked. Just unlink it.
    int rc = ::unlink((dir_path / _entry).c_str());
    FAIL_IF(rc != 0) << "Failed to unlink " << _target << " from " << dir_path / _entry << ": "
                     << ERR;

    // Mark this version as committed and return
    Version::setCommitted();
    return;
  }
}
