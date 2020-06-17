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

Resolution Env::resolvePath(shared_ptr<Command> cmd,
                            fs::path path,
                            AccessFlags flags,
                            fs::path base_path,
                            shared_ptr<Artifact> base) noexcept {
  ASSERT(base) << "Resolution was requested relative to a null base artifact";

  // If the path is absolute, strip off the leading slash
  if (path.is_absolute()) path = path.relative_path();

  // Is the path empty?
  if (path.begin() == path.end()) {
    // Yes. The resolution reaches the base artifact. Do we have access?
    if (base->checkAccess(cmd, flags)) {
      return base;
    } else {
      return EACCES;
    }
  }

  // If the base is not a directory, fail
  if (!base->as<DirArtifact>()) return ENOTDIR;

  // If we don't have execute access on the base part of the path, fail
  if (!base->checkAccess(cmd, {.x = true})) return EACCES;

  // Get the first part of the path, then advance the iterator so we can check for additional parts
  auto iter = path.begin();
  auto entry = *iter;
  iter++;

  // Try to get the named entry from base
  auto result = base->getEntry(base_path, entry);

  // If the resolution succeeded, updated the name for the resulting artifact
  if (result) result->setName((fs::path(base->getName()) / entry).lexically_normal());

  // Does the path have just one part?
  if (iter == path.end()) {
    // Yes. We have some extra work to do.

    // If this resolution was meant to create a file, fail with an error
    if (flags.create && flags.exclusive && result) return EEXIST;

    // If no matching file exists but this resolution can create it, do so
    if (flags.create && result == ENOENT) {
      // Make sure we have write access in the base directory
      if (!base->checkAccess(cmd, {.w = true})) return EACCES;

      // Create the file and give it a concise name
      auto newfile = createFile(base_path / entry, cmd, flags);
      newfile->setName((fs::path(base->getName()) / entry).lexically_normal());

      // Add the new file to the directory
      base->setEntry(entry, newfile);

      // Return the new file
      return newfile;
    }

    // Otherwise, if the directory access failed, return the error
    if (!result) return result;

    // If the result is a symlink, we may need to follow it
    if (auto symlink = result->as<SymlinkArtifact>(); symlink && !flags.nofollow) {
      auto symlink_path = symlink->readlink(cmd)->getDestination();
      if (symlink_path.is_relative()) {
        // Resolve the symlink relative to its containing directory
        result = resolvePath(cmd, symlink_path, flags, base_path, base);
      } else {
        // Resolve the symlink as an absolute path
        result = resolvePath(cmd, symlink_path, flags);
      }

      // If the symlink resolution failed, return the error
      if (!result) return result;
    }

    // Check the final artifact to see if we have permission to access it
    if (!result->checkAccess(cmd, flags)) return EACCES;

    // Success. Return the result
    return result;
  }

  // At this point, result holds the outcome of getting the next entry from the base directory

  // If that access failed, return failure
  if (!result) return result;

  // If the result is a symlink, we always follow it
  if (auto symlink = result->as<SymlinkArtifact>()) {
    auto symlink_path = symlink->readlink(cmd)->getDestination();
    if (symlink_path.is_relative()) {
      // Resolve the symlink relative to its containing directory
      result = resolvePath(cmd, symlink_path, flags, base_path, base);
    } else {
      // Resolve the symlink as an absolute path
      result = resolvePath(cmd, symlink_path, flags);
    }

    // If the symlink resolution failed, return the error
    if (!result) return result;
  }

  // fs::path doesn't have a convenient way to pull off the front component, so do that here
  fs::path newpath = *(iter++);
  while (iter != path.end()) {
    newpath /= *(iter++);
  }

  // Now make a recursive call to resolve the rest of the path
  return resolvePath(cmd, newpath, flags, base_path / entry, result);
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
    a = make_shared<DirArtifact>(*this, true, mv);

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
  auto cv = make_shared<ContentVersion>();
  cv->createdBy(creator);

  // Create the artifact and return it
  auto artifact = make_shared<FileArtifact>(*this, true, mv, cv);
  artifact->setName(path);

  // Observe output to metadata and content for the new file
  _build.observeOutput(creator, artifact, mv);
  _build.observeOutput(creator, artifact, cv);

  return artifact;
}

// Get a reference to the root directory
shared_ptr<Access> Env::getRootRef(AccessFlags flags) noexcept {
  auto ref = make_shared<Access>(nullptr, "/", flags);
  ref->resolvesTo(getRootDir());
  return ref;
}

// Check all remaining artifacts for changes and save updated fingerprints and metadata
void Env::finalize() noexcept {
  if (_root_dir) {
    auto root_ref = make_shared<Access>(nullptr, "/", AccessFlags{.x = true});
    root_ref->resolvesTo(_root_dir);
    _root_dir->finalize(root_ref);
  }
}
