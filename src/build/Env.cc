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
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::make_shared;
using std::map;
using std::shared_ptr;
using std::string;

Env::Env(Build& build) noexcept : _build(build) {
  struct stat info;
  int rc = ::lstat("/", &info);
  ASSERT(rc == 0) << "Failed to stat root directory";
  _root_dir = getArtifact("/", info)->as<DirArtifact>();
  _root_dir->setName("/");
  _root_dir->addLinkUpdate(nullptr, "/", nullptr);
}

fs::path Env::getTempPath() noexcept {
  // Make sure the temporary directory exsits
  fs::path tmpdir = ".dodo/tmp";
  fs::create_directories(".dodo/tmp");

  // Create a unique temporary path
  fs::path result;
  do {
    result = tmpdir / std::to_string(_next_temp_id++);
  } while (fs::exists(result));

  // Return the result
  return result;
}

shared_ptr<Artifact> Env::getArtifact(fs::path path, struct stat& info) {
  // Does the inode for this path match an artifact we've already created?
  auto inode_iter = _inodes.find({info.st_dev, info.st_ino});
  if (inode_iter != _inodes.end()) {
    // Found a match. Return it now.
    return inode_iter->second;
  }

  auto mv = make_shared<MetadataVersion>(info);
  mv->setCommitted();

  // Create a new artifact for this inode
  shared_ptr<Artifact> a;
  if ((info.st_mode & S_IFMT) == S_IFREG) {
    // The path refers to a regular file
    auto cv = make_shared<FileVersion>(info);
    cv->setCommitted();
    a = make_shared<FileArtifact>(*this, mv, cv);

  } else if ((info.st_mode & S_IFMT) == S_IFDIR) {
    // The path refers to a directory
    auto dv = make_shared<ExistingDir>();
    dv->setCommitted();
    a = make_shared<DirArtifact>(*this, mv, dv);

  } else if ((info.st_mode & S_IFMT) == S_IFLNK) {
    auto sv = make_shared<SymlinkVersion>(readlink(path));
    sv->setCommitted();
    a = make_shared<SymlinkArtifact>(*this, mv, sv);

  } else {
    // The path refers to something else
    WARN << "Unexpected filesystem node type at " << path << ". Treating it as a file.";
    auto cv = make_shared<FileVersion>(info);
    cv->setCommitted();
    a = make_shared<FileArtifact>(*this, mv, cv);
  }

  // Add the new artifact to the inode map
  _inodes.emplace_hint(inode_iter, pair{info.st_dev, info.st_ino}, a);

  // Return the artifact
  return a;
}

shared_ptr<PipeArtifact> Env::getPipe(shared_ptr<Command> c) noexcept {
  // Create a manufactured stat buffer for the new pipe
  uid_t uid = getuid();
  gid_t gid = getgid();
  mode_t mode = S_IFIFO | 0600;

  // Create initial versions and the pipe artifact
  auto mv = make_shared<MetadataVersion>(Metadata(uid, gid, mode));
  mv->setCommitted();

  auto cv = make_shared<FileVersion>(FileFingerprint::makeEmpty());
  cv->setCommitted();

  auto pipe = make_shared<PipeArtifact>(*this, mv, cv);

  // If a command was provided, report the outputs to the build
  if (c) {
    mv->createdBy(c);
    _build.observeOutput(c, pipe, mv);

    cv->createdBy(c);
    _build.observeOutput(c, pipe, cv);
  }

  _anonymous.insert(pipe);

  return pipe;
}

shared_ptr<SymlinkArtifact> Env::getSymlink(shared_ptr<Command> c,
                                            fs::path target,
                                            bool committed) noexcept {
  // Create a manufactured stat buffer for the new symlink
  uid_t uid = getuid();
  gid_t gid = getgid();
  mode_t mode = S_IFLNK | 0777;

  // Create initial versions and the pipe artifact
  auto mv = make_shared<MetadataVersion>(Metadata(uid, gid, mode));
  if (committed) mv->setCommitted();

  auto sv = make_shared<SymlinkVersion>(target);
  if (committed) sv->setCommitted();

  auto symlink = make_shared<SymlinkArtifact>(*this, mv, sv);

  // If a command was provided, report the outputs to the build
  if (c) {
    mv->createdBy(c);
    _build.observeOutput(c, symlink, mv);

    sv->createdBy(c);
    _build.observeOutput(c, symlink, sv);
  }

  _anonymous.insert(symlink);

  return symlink;
}

shared_ptr<DirArtifact> Env::getDir(shared_ptr<Command> c, mode_t mode, bool committed) noexcept {
  // Get the current umask
  auto mask = umask(0);
  umask(mask);

  // Create uid, gid, and mode values for this new directory
  uid_t uid = getuid();
  gid_t gid = getgid();
  mode_t stat_mode = S_IFDIR | (mode & ~mask);

  // Create initial versions
  auto mv = make_shared<MetadataVersion>(Metadata(uid, gid, stat_mode));
  if (committed) mv->setCommitted();

  auto dv = make_shared<CreatedDir>();
  if (committed) dv->setCommitted();

  auto dir = make_shared<DirArtifact>(*this, mv, dv);

  // If a command was provided, report the outputs to the build
  if (c) {
    mv->createdBy(c);
    _build.observeOutput(c, dir, mv);

    dv->createdBy(c);
    _build.observeOutput(c, dir, dv);
  }

  _anonymous.insert(dir);

  return dir;
}

shared_ptr<Artifact> Env::createFile(shared_ptr<Command> creator,
                                     AccessFlags flags,
                                     bool committed) noexcept {
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
  if (committed) mv->setCommitted();

  // Create an initial content version
  auto cv = make_shared<FileVersion>(FileFingerprint::makeEmpty());
  cv->createdBy(creator);
  if (committed) cv->setCommitted();

  // Create the artifact and return it
  auto artifact = make_shared<FileArtifact>(*this, mv, cv);

  // Observe output to metadata and content for the new file
  _build.observeOutput(creator, artifact, mv);
  _build.observeOutput(creator, artifact, cv);

  _anonymous.insert(artifact);

  return artifact;
}
