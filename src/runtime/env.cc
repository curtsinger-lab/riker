#include "env.hh"

#include <cstddef>
#include <filesystem>
#include <list>
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
#include "artifacts/SpecialArtifact.hh"
#include "artifacts/SymlinkArtifact.hh"
#include "runtime/Command.hh"
#include "util/log.hh"
#include "util/stats.hh"
#include "util/wrappers.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::list;
using std::make_shared;
using std::map;
using std::pair;
using std::set;
using std::shared_ptr;
using std::string;
using std::weak_ptr;

namespace fs = std::filesystem;

namespace env {
  /// The next unique ID for a temporary file
  size_t _next_temp_id = 0;

  // Special artifacts
  shared_ptr<Artifact> _stdin;        //< Standard input
  shared_ptr<Artifact> _stdout;       //< Standard output
  shared_ptr<Artifact> _stderr;       //< Standard error
  shared_ptr<DirArtifact> _root_dir;  //< The root directory

  /// A set of all the artifacts used during the build
  list<weak_ptr<Artifact>> _artifacts;

  /// A map of artifacts identified by inode
  map<pair<dev_t, ino_t>, weak_ptr<Artifact>> _inodes;

  // Reset the state of the environment by clearing all known artifacts
  void rollback() noexcept {
    _stdin.reset();
    _stdout.reset();
    _stderr.reset();
    if (_root_dir) _root_dir->rollback();
  }

  // Fingerprint and cache any versions on the filesystem
  void cacheAll() noexcept { getRootDir()->cacheAll("/"); }

  // Commit all changes to the filesystem
  void commitAll() noexcept { getRootDir()->applyFinalState("/"); }

  // Get the set of all artifacts
  const list<weak_ptr<Artifact>>& getArtifacts() noexcept { return _artifacts; }

  shared_ptr<Artifact> getStdin(const shared_ptr<Command>& c) noexcept {
    if (!_stdin) {
      // Create a manufactured stat buffer for stdin
      uid_t uid = getuid();
      gid_t gid = getgid();
      mode_t mode = S_IFIFO | 0600;

      // Create stdin
      auto a = make_shared<SpecialArtifact>(MetadataVersion(uid, gid, mode), true);
      _stdin = a;

      // Set the file descriptor and name
      a->setFD(0);
      a->setName("stdin");

      // Record stats for this artifact
      _artifacts.push_back(_stdin);
      stats::artifacts++;
    }

    return _stdin;
  }

  shared_ptr<Artifact> getStdout(const shared_ptr<Command>& c) noexcept {
    if (!_stdout) {
      // Create a manufactured stat buffer for stdin
      uid_t uid = getuid();
      gid_t gid = getgid();
      mode_t mode = S_IFIFO | 0600;

      // Create stdout
      auto a = make_shared<SpecialArtifact>(MetadataVersion(uid, gid, mode), false);
      _stdout = a;

      // Set the file descriptor and name
      a->setFD(1);
      a->setName("stdout");

      // Record stats for this artifact
      _artifacts.push_back(_stdout);
      stats::artifacts++;
    }

    return _stdout;
  }

  shared_ptr<Artifact> getStderr(const shared_ptr<Command>& c) noexcept {
    if (!_stderr) {
      // Create a manufactured stat buffer for stdin
      uid_t uid = getuid();
      gid_t gid = getgid();
      mode_t mode = S_IFIFO | 0600;

      // Create stderr
      auto a = make_shared<SpecialArtifact>(MetadataVersion(uid, gid, mode), false);
      _stderr = a;

      // Set the file descriptor and name
      a->setFD(2);
      a->setName("stderr");

      // Record stats for this artifact
      _artifacts.push_back(_stderr);
      stats::artifacts++;
    }

    return _stderr;
  }

  shared_ptr<DirArtifact> getRootDir() noexcept {
    if (!_root_dir) {
      auto a = getFilesystemArtifact("/");
      FAIL_IF(!a) << "Failed to get an artifact for the root directory";

      _root_dir = a->as<DirArtifact>();
      FAIL_IF(!_root_dir) << "Artifact at path \"/\" is not a directory";

      _root_dir->setRootDir();
      _root_dir->setName("/");
    }

    return _root_dir;
  }

  fs::path getTempPath() noexcept {
    // Make sure the temporary directory exsits
    fs::path tmpdir = ".rkr/tmp";
    fs::create_directories(".rkr/tmp");

    // Create a unique temporary path
    fs::path result;
    do {
      result = tmpdir / std::to_string(_next_temp_id++);
    } while (fs::exists(result));

    // Return the result
    return result;
  }

  /// A map from paths to special artifact parameters. A "true" value means the artifact is treated
  /// as always changed, and any writes cannot be committed. False means content is treated as if it
  /// never changes, any any writes from emulated commands can be treated as if they are committed.
  map<string, bool> special_artifacts = {{"/dev/null", false},
                                         {"/dev/urandom", false},
                                         {"/dev/tty", false}};

  /// A map from paths ot special artifact parameters, where any path that falls under the given
  /// directory is treated as a special artifact with the given flag.
  map<string, bool> special_artifact_dirs = {{"/dev/pts/", false}};

  shared_ptr<Artifact> getFilesystemArtifact(fs::path path) noexcept {
    // Stat the path on the filesystem to get the file type and an inode number
    struct stat info;
    int rc = ::lstat(path.c_str(), &info);

    // If the lstat call failed, the file does not exist
    if (rc != 0) return nullptr;

    // Does the inode for this path match an artifact we've already created?
    auto inode_iter = _inodes.find({info.st_dev, info.st_ino});
    if (inode_iter != _inodes.end()) {
      // Found a match. Is the weak pointer still valid?
      auto result = inode_iter->second.lock();
      if (result) return result;

      // Invalid weak pointer, so we can erase the entry
      _inodes.erase(inode_iter);
    }

    // Create a new artifact for this inode
    shared_ptr<Artifact> a;
    if ((info.st_mode & S_IFMT) == S_IFREG) {
      // The path refers to a regular file
      auto cv = make_shared<FileVersion>(info);
      a = make_shared<FileArtifact>(MetadataVersion(info), cv);

    } else if ((info.st_mode & S_IFMT) == S_IFDIR) {
      // The path refers to a directory
      auto dv = make_shared<BaseDirVersion>(false);
      a = make_shared<DirArtifact>(MetadataVersion(info), dv);

    } else if ((info.st_mode & S_IFMT) == S_IFLNK) {
      auto sv = make_shared<SymlinkVersion>(readlink(path));
      a = make_shared<SymlinkArtifact>(MetadataVersion(info), sv);

    } else {
      // Does the exact artifact path appear in the special artifacts map?
      if (auto iter = special_artifacts.find(path); iter != special_artifacts.end()) {
        a = make_shared<SpecialArtifact>(MetadataVersion(info), iter->second);
      }

      // Does the path begin with one of the special artifact directories?
      for (auto [prefix, always_changed] : special_artifact_dirs) {
        if (path.string().substr(0, prefix.size()) == prefix) {
          a = make_shared<SpecialArtifact>(MetadataVersion(info), always_changed);
          break;
        }
      }

      // The path refers to something else
      if (!a) {
        WARN << "Unexpected filesystem node type at " << path << ". Treating it as a file.";
        auto cv = make_shared<FileVersion>(info);
        a = make_shared<FileArtifact>(MetadataVersion(info), cv);
      }
    }

    // Add the new artifact to the inode map
    _inodes.emplace_hint(inode_iter, pair{info.st_dev, info.st_ino}, a);

    // Also add the artifact to the set of all artifacts
    _artifacts.push_back(a);
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
    pipe->updateMetadata(c, MetadataVersion(uid, gid, mode));

    _artifacts.push_back(pipe);
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
    symlink->updateMetadata(c, MetadataVersion(uid, gid, mode));
    symlink->updateContent(c, make_shared<SymlinkVersion>(target));

    _artifacts.push_back(symlink);
    stats::artifacts++;

    return symlink;
  }

  shared_ptr<DirArtifact> getDir(const shared_ptr<Command>& c, mode_t mode) noexcept {
    // Create uid, gid, and mode values for this new directory
    uid_t uid = getuid();
    gid_t gid = getgid();
    mode_t stat_mode = S_IFDIR | (mode & 0777);

    // Create a directory artifact
    auto dir = make_shared<DirArtifact>();

    // Initialize the directory content as an empty dir created by c
    dir->createEmptyDir(c);

    // Set the metadata for the new directory artifact
    dir->updateMetadata(c, MetadataVersion(uid, gid, stat_mode));

    _artifacts.push_back(dir);
    stats::artifacts++;

    return dir;
  }

  shared_ptr<Artifact> createFile(const shared_ptr<Command>& c, mode_t mode) noexcept {
    // Create uid, gid, and mode values for this new file
    uid_t uid = getuid();
    gid_t gid = getgid();
    mode_t stat_mode = S_IFREG | (mode & 0777);

    // Create an initial content version
    auto cv = make_shared<FileVersion>();
    cv->makeEmptyFingerprint();

    // Create the artifact and return it
    auto artifact = make_shared<FileArtifact>();

    // Set the metadata and content for the new file artifact
    artifact->updateMetadata(c, MetadataVersion(uid, gid, stat_mode));
    artifact->updateContent(c, cv);

    // Observe output to metadata and content for the new file
    c->addContentOutput(artifact, cv);

    _artifacts.push_back(artifact);
    stats::artifacts++;

    return artifact;
  }
}
