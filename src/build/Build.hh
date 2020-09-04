#pragma once

#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <tuple>
#include <vector>

#include "build/BuildObserver.hh"
#include "build/Env.hh"
#include "build/RebuildPlan.hh"
#include "build/Resolution.hh"
#include "core/Command.hh"
#include "core/IR.hh"
#include "core/Trace.hh"
#include "tracing/Tracer.hh"

using std::make_shared;
using std::optional;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::tuple;
using std::vector;

class Version;

/**
 * A Build instance manages the execution of a build. This instance is responsible for setting up
 * the build environment, emulating or running each of the commands, and notifying any observers of
 * dependencies and changes detected during the build.
 */
class Build {
 public:
  /**
   * Create a build runner
   */
  Build(shared_ptr<Trace> input_trace,
        RebuildPlan plan = RebuildPlan(),
        shared_ptr<Env> env = make_shared<Env>()) noexcept :
      _steps(input_trace->getSteps()),
      _trace(make_shared<Trace>()),
      _plan(plan),
      _env(env),
      _tracer(*this) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /**
   * Run a build trace in a given environment.
   * \returns a tuple of the new traces produced by the run, and the environment in its final state
   */
  tuple<shared_ptr<Trace>, shared_ptr<Env>> run() noexcept;

  /// Print information about this build
  ostream& print(ostream& o) const noexcept;

  /********** Emulate IR Steps **********/

  /// A command is issuing a reference to a special artifact (e.g. stdin, stdout, root dir)
  void emulateSpecialRef(shared_ptr<Command> c,
                         SpecialRef::Entity entity,
                         shared_ptr<RefResult> output) noexcept;

  /// A command references a new anonymous pipe
  void emulatePipeRef(shared_ptr<Command> c, shared_ptr<RefResult> output) noexcept;

  /// A command references a new anonymous file
  void emulateFileRef(shared_ptr<Command> c, mode_t mode, shared_ptr<RefResult> output) noexcept;

  /// A command references a new anonymous symlink
  void emulateSymlinkRef(shared_ptr<Command> c,
                         fs::path target,
                         shared_ptr<RefResult> output) noexcept;

  /// A command references a new anonymous directory
  void emulateDirRef(shared_ptr<Command> c, mode_t mode, shared_ptr<RefResult> output) noexcept;

  /// A command makes a reference with a path
  void emulatePathRef(shared_ptr<Command> c,
                      shared_ptr<RefResult> base,
                      fs::path path,
                      AccessFlags flags,
                      shared_ptr<RefResult> output) noexcept;

  /// A command expects a reference to resolve with a particular result
  void emulateExpectResult(shared_ptr<Command> c, shared_ptr<RefResult> ref, int expected) noexcept;

  /// A command accesses metadata for an artifact and expects to find a particular version
  void emulateMatchMetadata(shared_ptr<Command> c,
                            shared_ptr<RefResult> ref,
                            shared_ptr<MetadataVersion> expected) noexcept;

  /// A command accesses content for an artifact and expects to find a particular version
  void emulateMatchContent(shared_ptr<Command> c,
                           shared_ptr<RefResult> ref,
                           shared_ptr<Version> expected) noexcept;

  /// A command modifies the metadata for an artifact
  void emulateUpdateMetadata(shared_ptr<Command> c,
                             shared_ptr<RefResult>,
                             shared_ptr<MetadataVersion> written) noexcept;

  /// A command writes a new version to an artifact
  void emulateUpdateContent(shared_ptr<Command> c,
                            shared_ptr<RefResult> ref,
                            shared_ptr<Version> written) noexcept;

  /// A command is launching a child command
  void emulateLaunch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept;

  /// A command is joining with a child command
  void emulateJoin(shared_ptr<Command> c, shared_ptr<Command> child, int exit_status) noexcept;

  /// A command has exited with an exit code
  void emulateExit(shared_ptr<Command> c, int exit_status) noexcept;

  /********** Trace IR Steps **********/

  /// A traced command referenced a new anonymous pipe
  shared_ptr<RefResult> tracePipeRef(shared_ptr<Command> c) noexcept;

  /// A traced command referenced a new anonymous file
  shared_ptr<RefResult> traceFileRef(shared_ptr<Command> c, mode_t mode) noexcept;

  /// A traced command referenced a new anonymous symlink
  shared_ptr<RefResult> traceSymlinkRef(shared_ptr<Command> c, fs::path target) noexcept;

  /// A traced command referenced a new anonymous directory
  shared_ptr<RefResult> traceDirRef(shared_ptr<Command> c, mode_t mode) noexcept;

  /// A traced command referenced a path
  shared_ptr<RefResult> tracePathRef(shared_ptr<Command> c,
                                     shared_ptr<RefResult> base,
                                     fs::path path,
                                     AccessFlags flags) noexcept;

  /// A command expects a reference to resolve with a particular result
  void traceExpectResult(shared_ptr<Command> c, shared_ptr<RefResult> ref, int expected) noexcept;

  /// A command accesses metadata for an artifact and expects to find a particular version
  void traceMatchMetadata(shared_ptr<Command> c, shared_ptr<RefResult> ref) noexcept;

  /// A command accesses content for an artifact and expects to find a particular version
  void traceMatchContent(shared_ptr<Command> c, shared_ptr<RefResult> ref) noexcept;

  /// A command modifies the metadata for an artifact
  void traceUpdateMetadata(shared_ptr<Command> c, shared_ptr<RefResult>) noexcept;

  /// A command writes a new version to an artifact
  void traceUpdateContent(shared_ptr<Command> c,
                          shared_ptr<RefResult> ref,
                          shared_ptr<Version> written = nullptr) noexcept;

  /// A command is launching a child command
  void traceLaunch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept;

  /// A command is joining with a child command
  void traceJoin(shared_ptr<Command> c, shared_ptr<Command> child, int exit_status) noexcept;

  /// A command has exited with an exit code
  void traceExit(shared_ptr<Command> c, int exit_status) noexcept;

  /********** Observer Interface **********/

  /// Add an observer to this build
  Build& addObserver(shared_ptr<BuildObserver> o) noexcept {
    _observers.push_back(o);
    return *this;
  }

  /// Inform observers that a command has never run
  void observeCommandNeverRun(shared_ptr<Command> c) const noexcept;

  /// Inform observers that a parent command launched a child command
  void observeLaunch(shared_ptr<Command> parent, shared_ptr<Command> child) const noexcept;

  /// Inform observers that command c modified artifact a, creating version v
  void observeOutput(shared_ptr<Command> c,
                     shared_ptr<Artifact> a,
                     shared_ptr<Version> v) const noexcept;

  /// Inform observers that command c accessed version v of artifact a
  void observeInput(shared_ptr<Command> c,
                    shared_ptr<Artifact> a,
                    shared_ptr<Version> v,
                    InputType t) noexcept;

  /// Inform observers that command c did not find the expected version in artifact a
  /// Instead of version `expected`, the command found version `observed`
  void observeMismatch(shared_ptr<Command> c,
                       shared_ptr<Artifact> a,
                       shared_ptr<Version> observed,
                       shared_ptr<Version> expected) const noexcept;

  /// Inform observers that a given command's IR action would detect a change in the build env
  void observeCommandChange(shared_ptr<Command> c, shared_ptr<const Step> s) const noexcept;

  /// Inform observers that the version of an artifact produced during the build does not match the
  /// on-disk version.
  void observeFinalMismatch(shared_ptr<Artifact> a,
                            shared_ptr<Version> produced,
                            shared_ptr<Version> ondisk) const noexcept;

 private:
  /// Is a particular command running?
  bool isRunning(shared_ptr<Command> c) const noexcept {
    return _running.find(c) != _running.end();
  }

 private:
  /// The trace this build is running
  Trace::StepList _steps;

  /// The trace of steps performed by this build
  shared_ptr<Trace> _trace;

  /// The rebuild plan
  RebuildPlan _plan;

  /// The environment in which this build executes
  shared_ptr<Env> _env;

  /// The tracer that will be used to execute any commands that must rerun
  Tracer _tracer;

  /// A map of launched commands to the root process running that command, or nullptr if it is only
  /// being emulated
  map<shared_ptr<Command>, shared_ptr<Process>> _running;

  /// A set of commands that have exited
  set<shared_ptr<Command>> _exited;

  /// The observers that should be notified of dependency and change information during the build
  vector<shared_ptr<BuildObserver>> _observers;

  /// The last write performed by any command
  tuple<shared_ptr<Command>, shared_ptr<RefResult>, shared_ptr<Version>> _last_write;
};