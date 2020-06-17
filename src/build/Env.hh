#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <string>

#include <sys/types.h>

#include "build/BuildObserver.hh"
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
class Reference;

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
  Env(Build& build) noexcept : _build(build), _root_dir(getPath("/")) {}

  // Disallow Copy
  Env(const Env&) = delete;
  Env& operator=(const Env&) = delete;

  /**
   * Get the Build instance this environment is part of
   */
  Build& getBuild() const noexcept { return _build; }

  /**
   * Check and save data for any artifacts left in the environment.
   * This reports changes for artifacts whose on-disk versions do not match what the build
   * produced, and saves fingerprints and metadata for artifacts that were modified by executed
   * commands.
   */
  void finalize() noexcept;

  /**
   * Resolve a path relative to some base artifact
   * \param cmd   The command performing the resolution
   * \param path  The path to resolve
   * \param flags The access requirements for the artifact reached via the path
   * \param base_path The path to the directory where resolution should begin
   * \param base      The directory artifact where resolution should begin
   * \returns a resultion result, which holds either an artifact or an error code
   */
  Resolution resolvePath(shared_ptr<Command> cmd, fs::path path, AccessFlags flags,
                         fs::path base_path, shared_ptr<Artifact> base) noexcept;

  /**
   * Resolve a path relative to the root directory
   * \param cmd   The command performing the resolution
   * \param path  The path to resolve
   * \param flags The access requirements for the artifact reached via the path
   * \returns a resultion result, which holds either an artifact or an error code
   */
  Resolution resolvePath(shared_ptr<Command> cmd, fs::path path, AccessFlags flags) noexcept {
    return resolvePath(cmd, path, flags, "/", _root_dir);
  }

  /**
   * Create a pipe artifact
   * \param c The command that creates the pipe
   * \returns a pipe artifact
   */
  shared_ptr<PipeArtifact> getPipe(shared_ptr<Command> c) noexcept;

  /**
   * Get an artifact corresponding to a path on the filesystem
   * \param path  The path to check
   * \returns an artifact, or nullptr if no path exists
   */
  shared_ptr<Artifact> getPath(fs::path path) noexcept;

  /**
   * Get the root directory
   */
  shared_ptr<Artifact> getRootDir() const noexcept { return _root_dir; }

  /**
   * Get a reference to the root directory
   */
  shared_ptr<Access> getRootRef(AccessFlags flags = {.x = true}) noexcept;

  /**
   * Create a file artifact that exists only in the filesystem model
   * \param path    The path where this file will eventually appear
   * \param creator The command that creates this file
   * \returns a file artifact
   */
  shared_ptr<Artifact> createFile(fs::path path, shared_ptr<Command> creator,
                                  AccessFlags flags) noexcept;

 private:
  /// The build this environment is attached to
  Build& _build;

  /// A map of artifacts identified by inode
  map<pair<dev_t, ino_t>, shared_ptr<Artifact>> _inodes;

  /// An artifact that corresponds to the root directory
  shared_ptr<Artifact> _root_dir;
};
