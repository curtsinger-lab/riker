#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <string>

#include <sys/types.h>

#include "artifacts/DirArtifact.hh"
#include "build/BuildObserver.hh"
#include "build/Resolution.hh"
#include "core/AccessFlags.hh"
#include "core/IR.hh"

using std::map;
using std::shared_ptr;
using std::string;

namespace fs = std::filesystem;

class Access;
class Artifact;
class Build;
class Command;
class Pipe;
class PipeArtifact;
class Ref;
class SymlinkArtifact;

/**
 * An Env instance represents the environment where a build process executes. This captures all of
 * the files, directories, and pipes that the build process interacts with. The primary job of the
 * Env is to produce artifacts to model each of these entities in response to accesses from traced
 * or emulated commands.
 */
class Env {
 public:
  /**
   * Create an environment for build emulation or execution.
   * \param build The build that executes in this environment
   */
  Env(Build& build) noexcept;

  // Disallow Copy
  Env(const Env&) = delete;
  Env& operator=(const Env&) = delete;

  /// Get the Build instance this environment is part of
  Build& getBuild() const noexcept { return _build; }

  /// Get the root directory
  shared_ptr<DirArtifact> getRootDir() const noexcept { return _root_dir; }

  /**
   * Get an artifact to represent a statted file/dir/pipe/symlink.
   * If an artifact with the same inode and device number already exists, return that same instance.
   * \param path  The path to this artifact (currently only used to read symlinks)
   * \param dir   The directory that contains this artifact
   * \param info  The stat results
   * \returns an artifact pointer
   */
  shared_ptr<Artifact> getArtifact(fs::path path, struct stat& info);

  /**
   * Create a pipe artifact
   * \param c The command that creates the pipe
   * \returns a pipe artifact
   */
  shared_ptr<PipeArtifact> getPipe(shared_ptr<Command> c) noexcept;

  /**
   * Create a symlink artifact
   * \param c         The command creating the symlink
   * \param target    The destination of the symlink
   * \param committed If true, the symlink is already committed
   * \returns a symlink artifact
   */
  shared_ptr<SymlinkArtifact> getSymlink(shared_ptr<Command> c,
                                         fs::path target,
                                         bool committed) noexcept;

  /**
   * Create a directory artifact
   * \param c         The command that is creating the directory
   * \param mode      The mode (permissions) specified when creating the directory
   * \param committed If true, the directory is already committed
   * \returns a directory artifact
   */
  shared_ptr<DirArtifact> getDir(shared_ptr<Command> c, mode_t mode, bool committed) noexcept;

  /**
   * Create a file artifact that exists only in the filesystem model
   * \param path    The path where this file will eventually appear
   * \param creator The command that creates this file
   * \returns a file artifact
   */
  shared_ptr<Artifact> createFile(shared_ptr<Command> creator,
                                  AccessFlags flags,
                                  bool committed) noexcept;

 private:
  /// The build this environment is attached to
  Build& _build;

  /// An artifact that corresponds to the root directory
  shared_ptr<DirArtifact> _root_dir;

  /// A set of artifacts without known inodes
  set<shared_ptr<Artifact>> _anonymous;

  /// A map of artifacts identified by inode
  map<pair<dev_t, ino_t>, shared_ptr<Artifact>> _inodes;
};
