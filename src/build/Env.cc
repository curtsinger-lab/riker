#include "Env.hh"

#include <map>
#include <memory>
#include <string>
#include <tuple>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "artifacts/Dir.hh"
#include "artifacts/File.hh"
#include "artifacts/Pipe.hh"
#include "build/Build.hh"
#include "core/Command.hh"
#include "core/IR.hh"
#include "ui/options.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/Version.hh"

using std::dynamic_pointer_cast;
using std::make_shared;
using std::map;
using std::shared_ptr;
using std::string;
using std::tuple;

shared_ptr<PipeArtifact> Env::getPipe(shared_ptr<Command> c) noexcept {
  auto mv = make_shared<MetadataVersion>();
  mv->createdBy(c);

  auto cv = make_shared<ContentVersion>();
  cv->createdBy(c);

  return make_shared<PipeArtifact>(*this, true, mv, cv);
}

shared_ptr<Artifact> Env::getPath(fs::path path) noexcept {
  // Try to stat the path
  struct stat statbuf;
  int rc = ::stat(path.c_str(), &statbuf);

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
  if (statbuf.st_mode & S_IFREG) {
    // The path refers to a regular file
    auto cv = make_shared<ContentVersion>(statbuf);
    a = make_shared<FileArtifact>(*this, true, mv, cv);

  } else if (statbuf.st_mode & S_IFDIR) {
    // The path refers to a directory
    a = make_shared<DirArtifact>(*this, true, mv);

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

shared_ptr<Artifact> Env::createFile(fs::path path, shared_ptr<Command> creator,
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
  ASSERT(mv->isSaved()) << "UH OH";

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

// Get the root directory artifact
shared_ptr<Artifact> Env::getRootDir() noexcept {
  if (!_root_dir) _root_dir = getPath("/");
  return _root_dir;
}

// Check all remaining artifacts for changes and save updated fingerprints and metadata
void Env::finalize() noexcept {
  if (_root_dir) {
    auto root_ref = make_shared<Access>(nullptr, "/", AccessFlags{.x = true});
    root_ref->resolvesTo(_root_dir, SUCCESS);
    _root_dir->finalize(root_ref);
  }
}
