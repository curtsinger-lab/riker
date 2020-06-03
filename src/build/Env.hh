#pragma once

#include <map>
#include <memory>
#include <string>
#include <tuple>

#include <sys/types.h>

#include "build/BuildObserver.hh"

using std::map;
using std::shared_ptr;
using std::string;
using std::tuple;

class Access;
class Artifact;
class Build;
class Command;
class Pipe;
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
  Env(Build* build) : _build(build) {}

  // Disallow Copy
  Env(const Env&) = delete;
  Env& operator=(const Env&) = delete;

  // Allow Move
  Env(Env&&) = default;
  Env& operator=(Env&&) = default;

  /**
   * Reset this environment to a safe starting state
   */
  void reset();

  /**
   * Get an artifact from this environment
   * \param c   The command that makes this access
   * \param ref The reference used to reach the artifact
   * \returns an artifact, a result code, and whether this access created the artifact
   */
  tuple<shared_ptr<Artifact>, int, bool> get(shared_ptr<Command> c, shared_ptr<Reference> ref);

  /**
   * Get a pipe from this environment
   * \param c   The command that makes this access
   * \param ref The pipe reference used to reach the artifact
   * \returns an artifact, a result code, and whether this access created the artifact
   */
  tuple<shared_ptr<Artifact>, int, bool> getPipe(shared_ptr<Command> c, shared_ptr<Pipe> ref);

  /**
   * Get a file from this environment
   * \param c   The command that makes this access
   * \param ref The reference used to reach the artifact
   * \returns an artifact, a result code, and whether this access created the artifact
   */
  tuple<shared_ptr<Artifact>, int, bool> getFile(shared_ptr<Command> c, shared_ptr<Access> ref);

  /**
   * Check and save data for any artifacts left in the environment.
   * This reports changes for artifacts whose on-disk versions do not match what the build produced,
   * and saves fingerprints and metadata for artifacts that were modified by executed commands.
   */
  void finalize();

 private:
  /// The build this environment is attached to
  Build* _build;

  /// An emulated filesystem for artifacts in this environment
  map<string, shared_ptr<Artifact>> _filesystem;

  /// The file artifacts that have been resolved in this artifact
  map<shared_ptr<Access>, shared_ptr<Artifact>> _files;

  /// The pipe artifacts used in this environment
  map<shared_ptr<Pipe>, shared_ptr<Artifact>> _pipes;
};
