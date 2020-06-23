#include "Env.hh"

#include <map>
#include <memory>
#include <string>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/FileArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "artifacts/SymlinkArtifact.hh"
#include "build/Build.hh"
#include "core/Command.hh"
#include "core/IR.hh"
#include "ui/options.hh"
#include "util/log.hh"
#include "util/path.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::make_shared;
using std::map;
using std::shared_ptr;
using std::string;

Resolution Env::resolveRef(shared_ptr<Command> cmd, shared_ptr<Access> ref) noexcept {
  ASSERT(ref->getBase()) << "Cannot resolve a reference relative to a null base";
  ASSERT(ref->getBase()->isResolved()) << "Cannot resolve a reference with an unresolved base";

  // Get the relative part of the path that we need to resolve
  auto path = ref->getRelativePath();
  auto flags = ref->getFlags();

  // Resolution begins at the base of the reference
  auto dir_ref = ref->getBase();
  auto dir = dir_ref->getArtifact();

  // If the path is empty, check access and return the base
  if (path.empty()) {
    if (dir->checkAccess(cmd, ref->getFlags())) {
      return dir;
    } else {
      return EACCES;
    }
  }

  // Loop over the pieces of the path
  auto path_iter = path.begin();
  while (path_iter != path.end()) {
    // Get the next part of the path
    auto entry = *path_iter;
    path_iter++;

    // Make sure we have permission to access the directory
    if (!dir->checkAccess(cmd, {.x = true})) return EACCES;

    // Try to get the next entry
    auto result = dir->getEntry(cmd, dir_ref, entry);

    // If the resolution succeeded, update the name of the resolved artifact
    if (result) result->setName((fs::path(dir->getName()) / entry).lexically_normal());

    // If result returned a symlink, we may need to resolve it
    if (result) {
      auto symlink = result->as<SymlinkArtifact>();
      if (symlink && (path_iter != path.end() || !ref->getFlags().nofollow)) {
        // Get the symlink path
        auto symlink_path = symlink->getSymlink(cmd, InputType::PathResolution)->getDestination();

        // If the symlink path is absolute, resolution continues relative to /
        if (symlink_path.is_absolute()) {
          dir = getRootDir();
          dir_ref = make_shared<Access>(nullptr, "/", AccessFlags{});
          dir_ref->resolvesTo(dir);
        }

        // Resolution will continue with a new path. That path is the symlink path, followed by the
        // remaining elements from the existing path
        fs::path new_path = symlink_path;
        while (path_iter != path.end()) {
          new_path /= *path_iter;
          path_iter++;
        }

        // Swap in the new path, and start the iterator at its beginning
        path = new_path;
        path_iter = path.begin();

        // Jump back to the top of the loop to begin resolving the new path
        continue;
      }
    }

    // Is this entry the last part of the path?
    if (path_iter == path.end()) {
      // If this resolution was required to create a file, but one already exists, fail
      if (flags.create && flags.exclusive && result) return EEXIST;

      // If no matching file exists but this resolution can create it, do so
      if (flags.create && result == ENOENT) {
        // Make sure we have write access into the directory
        if (!dir->checkAccess(cmd, {.w = true})) return EACCES;

        // Create the new file and give it a name
        auto newfile = createFile(dir_ref->getFullPath() / entry, cmd, flags);
        newfile->setName((fs::path(dir->getName()) / entry).lexically_normal());

        // Record the resolution to the new file
        ref->resolvesTo(newfile);

        // Add the new file to its directory
        dir->addEntry(cmd, dir_ref, entry, ref);

        // Return the resolution result
        return newfile;
      }

      // If the entry exists, make sure we have access
      if (result && !result->checkAccess(cmd, flags)) return EACCES;

      // Return the result
      return result;
    }

    // This is not the last step along the path. If it failed, return the error
    if (!result) return result;

    // Otherwise, advance the current directory and directory reference
    dir = result;
    dir_ref = make_shared<Access>(dir_ref, entry, AccessFlags{});
    dir_ref->resolvesTo(dir);
  }

  FAIL << "Unreachable";
  return ENOENT;
}

shared_ptr<PipeArtifact> Env::getPipe(shared_ptr<Command> c) noexcept {
  // Create a manufactured stat buffer for the new pipe
  uid_t uid = getuid();
  gid_t gid = getgid();
  mode_t mode = S_IFIFO | 0600;

  // Create initial versions and the pipe artifact
  auto mv = make_shared<MetadataVersion>(Metadata(uid, gid, mode));
  auto cv = make_shared<ContentVersion>(ContentFingerprint::makeEmpty());
  auto pipe = make_shared<PipeArtifact>(*this, true, mv, cv);

  // If a command was provided, report the outputs to the build
  if (c) {
    mv->createdBy(c);
    _build.observeOutput(c, pipe, mv);

    cv->createdBy(c);
    _build.observeOutput(c, pipe, cv);
  }

  return pipe;
}

shared_ptr<Artifact> Env::getPath(fs::path path) noexcept {
  // Try to stat the path
  struct stat statbuf;
  int rc = ::lstat(path.c_str(), &statbuf);

  // If stat failed, there is no artifact
  if (rc) return nullptr;

  // Does the inode for this path match an artifact we've already created?
  auto inode_iter = _inodes.find({statbuf.st_dev, statbuf.st_ino});
  if (inode_iter != _inodes.end()) {
    // Found a match. Return it now.
    inode_iter->second->setName(path);
    return inode_iter->second;
  }

  auto mv = make_shared<MetadataVersion>(statbuf);

  // Create a new artifact for this inode
  shared_ptr<Artifact> a;
  if ((statbuf.st_mode & S_IFMT) == S_IFREG) {
    // The path refers to a regular file
    auto cv = make_shared<ContentVersion>(statbuf);
    a = make_shared<FileArtifact>(*this, true, mv, cv);

  } else if ((statbuf.st_mode & S_IFMT) == S_IFDIR) {
    // The path refers to a directory
    auto dv = make_shared<ExistingDirVersion>();
    a = make_shared<DirArtifact>(*this, true, mv, dv);

  } else if ((statbuf.st_mode & S_IFMT) == S_IFLNK) {
    auto sv = make_shared<SymlinkVersion>(readlink(path));
    a = make_shared<SymlinkArtifact>(*this, true, mv, sv);

  } else {
    // The path refers to something else
    WARN << "Unexpected filesystem node type at " << path << ". Treating it as a file.";
    auto cv = make_shared<ContentVersion>(statbuf);
    a = make_shared<FileArtifact>(*this, true, mv, cv);
  }

  // Add the new artifact to the inode map
  _inodes.emplace_hint(inode_iter, pair{statbuf.st_dev, statbuf.st_ino}, a);

  a->setName(path);

  // Return the artifact
  return a;
}

shared_ptr<Artifact> Env::createFile(fs::path path,
                                     shared_ptr<Command> creator,
                                     AccessFlags flags) noexcept {
  // Get the current umask
  auto mask = umask(0);
  umask(mask);

  // Create uid, gid, and mode values for this new file
  uid_t uid = getuid();
  gid_t gid = getgid();
  mode_t mode = S_IFREG | (flags.mode & ~mask);

  // Create an initial metadata version
  auto mv = make_shared<MetadataVersion>(Metadata(uid, gid, mode));
  mv->createdBy(creator);

  // Create an initial content version
  auto cv = make_shared<ContentVersion>(ContentFingerprint::makeEmpty());
  cv->createdBy(creator);

  // Create the artifact and return it
  auto artifact = make_shared<FileArtifact>(*this, true, mv, cv);
  artifact->setName(path);

  // Observe output to metadata and content for the new file
  _build.observeOutput(creator, artifact, mv);
  _build.observeOutput(creator, artifact, cv);

  return artifact;
}

// Check all remaining artifacts for changes and save updated fingerprints and metadata
void Env::finalize() noexcept {
  if (_root_dir) {
    auto root_ref = make_shared<Access>(nullptr, "/", AccessFlags{.x = true});
    root_ref->resolvesTo(_root_dir);
    _root_dir->finalize(root_ref);
  }
}
