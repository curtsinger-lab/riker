#pragma once

#include <filesystem>
#include <list>
#include <memory>
#include <set>

#include <sys/types.h>

namespace fs = std::filesystem;

class Artifact;
class Command;
class DirArtifact;
class PipeArtifact;
class SymlinkArtifact;

/**
 * The env namespace holds all of the operations that can be performed on the model of the
 * filesystem. There is only one environment, but it can be reset to match filesystem state.
 */
namespace env {
  /// Reset the environment to match filesystem state
  void rollback() noexcept;

  /// Commit all changes in the environment to the filesystem
  void commitAll() noexcept;

  /// Get the standard input pipe
  std::shared_ptr<Artifact> getStdin(const std::shared_ptr<Command>& c) noexcept;

  /// Get the standard output pipe
  std::shared_ptr<Artifact> getStdout(const std::shared_ptr<Command>& c) noexcept;

  /// Get the standard error pipe
  std::shared_ptr<Artifact> getStderr(const std::shared_ptr<Command>& c) noexcept;

  /// Get the root directory
  std::shared_ptr<DirArtifact> getRootDir() noexcept;

  /// Get a unique path to a temporary file in the build directory
  fs::path getTempPath() noexcept;

  /// Get a set of all the artifacts in the build
  const std::list<std::weak_ptr<Artifact>>& getArtifacts() noexcept;

  /**
   * Get an artifact to represent a statted file/dir/pipe/symlink.
   * If an artifact with the same inode and device number already exists, return that same instance.
   * \param path  The path to this artifact on the filesystem
   * \returns an artifact pointer
   */
  std::shared_ptr<Artifact> getFilesystemArtifact(fs::path path) noexcept;

  /**
   * Create a pipe artifact
   * \param c The command that creates the pipe
   * \returns a pipe artifact
   */
  std::shared_ptr<PipeArtifact> getPipe(const std::shared_ptr<Command>& c) noexcept;

  /**
   * Create a symlink artifact
   * \param c         The command creating the symlink
   * \param target    The destination of the symlink
   * \param committed If true, the symlink is already committed
   * \returns a symlink artifact
   */
  std::shared_ptr<SymlinkArtifact> getSymlink(const std::shared_ptr<Command>& c,
                                              fs::path target) noexcept;

  /**
   * Create a directory artifact
   * \param c         The command that is creating the directory
   * \param mode      The mode (permissions) specified when creating the directory
   * \param committed If true, the directory is already committed
   * \returns a directory artifact
   */
  std::shared_ptr<DirArtifact> getDir(const std::shared_ptr<Command>& c, mode_t mode) noexcept;

  /**
   * Create a file artifact that exists only in the filesystem model
   * \param creator   The command that creates this file
   * \param mode      The permission bits to be set on the new file (will be modified by umask)
   * \param committed If true, the file is already committed
   * \returns a file artifact
   */
  std::shared_ptr<Artifact> createFile(const std::shared_ptr<Command>& creator,
                                       mode_t mode) noexcept;
}
