#pragma once

#include <functional>
#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include <sys/types.h>

#include "util/BuildObserver.hh"

using std::map;
using std::reference_wrapper;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

class Access;
class Artifact;
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
   * \param observer This build observer will be notified of dependencies and changes as the build
   *                 unfolds.
   */
  Env(BuildObserver& observer) : _observer(observer) {}

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

  /// Get an iterable view of the artifacts currently in this environment.
  const map<shared_ptr<Access>, shared_ptr<Artifact>>& getArtifacts() { return _files; }

  /**
   * Check the final state of all artifacts against the file system.
   * Report any mismatched contents or metadata to the observer
   */
  void checkFinalState();

  /********** Observer Interface **********/

  /// Inform the observer that command c modified the metadata of artifact a
  void observeMetadataOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    _observer.get().metadataOutput(c, a);
  }

  /// Inform the observer that command c modified the contents of artifact a
  void observeContentOutput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    _observer.get().contentOutput(c, a);
  }

  /// Inform the observer that command c accessed the metadata of artifact a
  void observeMetadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    _observer.get().metadataInput(c, a);
  }

  /// Inform the observer that command c accessed the contents of artifact a
  void observeContentInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    _observer.get().contentInput(c, a);
  }

  /// Inform the observer that command c did not find the expected metadata in artifact a
  void observeMetadataMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    _observer.get().metadataMismatch(c, a);
  }

  /// Inform the observer that command c did not find the expected contents in artifact a
  void observeContentMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {
    _observer.get().contentMismatch(c, a);
  }

  /// Inform the observer that a given command's IR action would detect a change in the build env
  void observeCommandChange(shared_ptr<Command> c, shared_ptr<const Step> s) {
    _observer.get().commandChanged(c, s);
  }

  /// Inform the observer that a command has launched another command
  void observeLaunch(shared_ptr<Command> parent, shared_ptr<Command> child) {
    _observer.get().launched(parent, child);
  }

  /// Inform the observer that an artifact's metadata does not match the expected final state
  void observeFinalMetadataMismatch(shared_ptr<Artifact> a) {
    _observer.get().finalMetadataMismatch(a);
  }

  /// Inform the observer that an artifact's contents do not match the expected final state
  void observeFinalContentMismatch(shared_ptr<Artifact> a) {
    _observer.get().finalContentMismatch(a);
  }

 private:
  /// An emulated filesystem for artifacts in this environment
  map<string, shared_ptr<Artifact>> _filesystem;

  /// The file artifacts that have been resolved in this artifact
  map<shared_ptr<Access>, shared_ptr<Artifact>> _files;

  /// The pipe artifacts used in this environment
  map<shared_ptr<Pipe>, shared_ptr<Artifact>> _pipes;

  /// An observer who should be notified of dependency and change events
  reference_wrapper<BuildObserver> _observer;
};
