#include "env.hh"

#include <cstddef>
#include <filesystem>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <utility>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/FileArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "artifacts/SymlinkArtifact.hh"
#include "runtime/Command.hh"
#include "ui/stats.hh"
#include "util/log.hh"
#include "util/wrappers.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::make_shared;
using std::map;
using std::pair;
using std::set;
using std::shared_ptr;

namespace fs = std::filesystem;

namespace env {
  /// The next unique ID for a temporary file
  size_t _next_temp_id = 0;

  // Special artifacts
  shared_ptr<PipeArtifact> _stdin;    //< Standard input
  shared_ptr<PipeArtifact> _stdout;   //< Standard output
  shared_ptr<PipeArtifact> _stderr;   //< Standard error
  shared_ptr<DirArtifact> _root_dir;  //< The root directory

  /// A set of all the artifacts used during the build
  set<shared_ptr<Artifact>> _artifacts;

  /// A map of artifacts identified by inode
  map<pair<dev_t, ino_t>, shared_ptr<Artifact>> _inodes;

  // Reset the state of the environment by clearing all known artifacts
  void reset() noexcept {
    _stdin = nullptr;
    _stdout = nullptr;
    _stderr = nullptr;
    _root_dir = nullptr;
    _artifacts.clear();
    _inodes.clear();
  }

  // Commit all changes to the filesystem
  void commitAll() noexcept { getRootDir()->applyFinalState("/"); }

  // Get the set of all artifacts
  const set<shared_ptr<Artifact>>& getArtifacts() noexcept { return _artifacts; }

  shared_ptr<PipeArtifact> getStdin(const shared_ptr<Command>& c) noexcept {
    if (!_stdin) {
      _stdin = getPipe(c);
      _stdin->setFDs(0, -1);
      _stdin->setName("stdin");
    }
    return _stdin;
  }

  shared_ptr<PipeArtifact> getStdout(const shared_ptr<Command>& c) noexcept {
    if (!_stdout) {
      _stdout = getPipe(c);
      _stdout->setFDs(-1, 1);
      _stdout->setName("stdout");
    }
    return _stdout;
  }

  shared_ptr<PipeArtifact> getStderr(const shared_ptr<Command>& c) noexcept {
    if (!_stderr) {
      _stderr = getPipe(c);
      _stderr->setFDs(-1, 2);
      _stderr->setName("stderr");
    }
    return _stderr;
  }

  shared_ptr<DirArtifact> getRootDir() noexcept {
    if (!_root_dir) {
      auto a = getFilesystemArtifact("/");
      FAIL_IF(!a) << "Failed to get an artifact for the root directory";

      _root_dir = a->as<DirArtifact>();
      FAIL_IF(!_root_dir) << "Artifact at path \"/\" is not a directory";

      _root_dir->setName("/");
      _root_dir->addLinkUpdate(nullptr, "/", nullptr);
    }

    return _root_dir;
  }

  fs::path getTempPath() noexcept {
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

  set<fs::path> ignored_artifacts = {"/dev/null"};

  shared_ptr<Artifact> getFilesystemArtifact(fs::path path) noexcept {
    // Stat the path on the filesystem to get the file type and an inode number
    struct stat info;
    int rc = ::lstat(path.c_str(), &info);

    // If the lstat call failed, the file does not exist
    if (rc != 0) return nullptr;

    // Does the inode for this path match an artifact we've already created?
    auto inode_iter = _inodes.find({info.st_dev, info.st_ino});
    if (inode_iter != _inodes.end()) {
      // Found a match. Return it now.
      return inode_iter->second;
    }

    auto mv = make_shared<MetadataVersion>(info);

    // Create a new artifact for this inode
    shared_ptr<Artifact> a;
    if (ignored_artifacts.find(path) != ignored_artifacts.end()) {
      // The provided path is in our set of ignored paths. For now, just track it as a file.
      auto cv = make_shared<FileVersion>(info);
      cv->setCommitted();
      a = make_shared<FileArtifact>(mv, cv);

    } else if ((info.st_mode & S_IFMT) == S_IFREG) {
      // The path refers to a regular file
      auto cv = make_shared<FileVersion>(info);
      cv->setCommitted();
      a = make_shared<FileArtifact>(mv, cv);

    } else if ((info.st_mode & S_IFMT) == S_IFDIR) {
      // The path refers to a directory
      auto dv = make_shared<BaseDirVersion>(false);
      dv->setCommitted();
      a = make_shared<DirArtifact>(mv, dv);

    } else if ((info.st_mode & S_IFMT) == S_IFLNK) {
      auto sv = make_shared<SymlinkVersion>(readlink(path));
      sv->setCommitted();
      a = make_shared<SymlinkArtifact>(mv, sv);

    } else {
      // The path refers to something else
      WARN << "Unexpected filesystem node type at " << path << ". Treating it as a file.";
      auto cv = make_shared<FileVersion>(info);
      cv->setCommitted();
      a = make_shared<FileArtifact>(mv, cv);
    }

    // Add the new artifact to the inode map
    _inodes.emplace_hint(inode_iter, pair{info.st_dev, info.st_ino}, a);

    // Also add the artifact to the set of all artifacts
    _artifacts.insert(a);
    stats::artifacts++;

    // Return the artifact
    return a;
  }

  shared_ptr<PipeArtifact> getPipe(const shared_ptr<Command>& c) noexcept {
    // Create a manufactured stat buffer for the new pipe
    uid_t uid = getuid();
    gid_t gid = getgid();
    mode_t mode = S_IFIFO | 0600;

    // Create the pipe artifact
    auto pipe = make_shared<PipeArtifact>();

    // Set the pipe's metadata on behalf of the command
    pipe->updateMetadata(c, make_shared<MetadataVersion>(uid, gid, mode));

    _artifacts.insert(pipe);
    stats::artifacts++;

    return pipe;
  }

  shared_ptr<SymlinkArtifact> getSymlink(const shared_ptr<Command>& c, fs::path target) noexcept {
    // Create a manufactured stat buffer for the new symlink
    uid_t uid = getuid();
    gid_t gid = getgid();
    mode_t mode = S_IFLNK | 0777;

    // Create the symlink artifact
    auto symlink = make_shared<SymlinkArtifact>();

    // Set the metadata for the new symlink artifact
    symlink->updateMetadata(c, make_shared<MetadataVersion>(uid, gid, mode));
    symlink->updateContent(c, make_shared<SymlinkVersion>(target));

    _artifacts.insert(symlink);
    stats::artifacts++;

    return symlink;
  }

  shared_ptr<DirArtifact> getDir(const shared_ptr<Command>& c, mode_t mode) noexcept {
    // Get the current umask
    auto mask = umask(0);
    umask(mask);

    // Create uid, gid, and mode values for this new directory
    uid_t uid = getuid();
    gid_t gid = getgid();
    mode_t stat_mode = S_IFDIR | (mode & ~mask);

    // Create the directory content version
    auto dv = make_shared<BaseDirVersion>(true);
    if (c->running()) dv->setCommitted();

    auto dir = make_shared<DirArtifact>(dv);

    // Set the metadata for the new directory artifact
    dir->updateMetadata(c, make_shared<MetadataVersion>(uid, gid, stat_mode));

    // If a command was provided, record the content output
    if (c) {
      dv->createdBy(c);
      c->addContentOutput(dir, dv);
    }

    _artifacts.insert(dir);
    stats::artifacts++;

    return dir;
  }

  shared_ptr<Artifact> createFile(const shared_ptr<Command>& c, mode_t mode) noexcept {
    // Get the current umask
    auto mask = umask(0);
    umask(mask);

    // Create uid, gid, and mode values for this new file
    uid_t uid = getuid();
    gid_t gid = getgid();
    mode_t stat_mode = S_IFREG | (mode & ~mask);

    // Create an initial content version
    auto cv = make_shared<FileVersion>();
    cv->makeEmptyFingerprint();

    // Create the artifact and return it
    auto artifact = make_shared<FileArtifact>();

    // Set the metadata and content for the new file artifact
    artifact->updateMetadata(c, make_shared<MetadataVersion>(uid, gid, stat_mode));
    artifact->updateContent(c, cv);

    // Observe output to metadata and content for the new file
    c->addContentOutput(artifact, cv);

    _artifacts.insert(artifact);
    stats::artifacts++;

    return artifact;
  }
}
