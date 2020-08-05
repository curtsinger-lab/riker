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
  Build(shared_ptr<Trace> trace,
        RebuildPlan plan = RebuildPlan(),
        shared_ptr<Env> env = make_shared<Env>()) noexcept :
      _steps(trace->getSteps()), _trace(trace->restart()), _plan(plan), _env(env), _tracer(*this) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /**
   * Run a build trace in a given environment.
   * \returns a tuple of the new traces produced by the run, and the environment in its final state
   */
  tuple<shared_ptr<Trace>, shared_ptr<Env>> run() noexcept;

  /****** Tracing and Emulation Methods ******/

  /// A command creates a new pipe
  shared_ptr<Pipe> pipe(shared_ptr<Command> c, shared_ptr<Pipe> emulating = nullptr) noexcept;

  /// A command creates a new file
  shared_ptr<File> file(shared_ptr<Command> c,
                        mode_t mode,
                        shared_ptr<File> emulating = nullptr) noexcept;

  /// A command creates a new symbolic link
  shared_ptr<Symlink> symlink(shared_ptr<Command> c,
                              fs::path target,
                              shared_ptr<Symlink> emulating = nullptr) noexcept;

  /// A command creates a new directory
  shared_ptr<Dir> dir(shared_ptr<Command> c,
                      mode_t mode,
                      shared_ptr<Dir> emulating = nullptr) noexcept;

  /// A command makes a reference with a path
  shared_ptr<Access> access(shared_ptr<Command> c,
                            shared_ptr<Access> base,
                            fs::path path,
                            AccessFlags flags,
                            shared_ptr<Access> emulating = nullptr) noexcept;

  /// A command accesses metadata for an artifact and expects to find a particular version
  void matchMetadata(shared_ptr<Command> c,
                     shared_ptr<Ref> ref,
                     shared_ptr<MetadataVersion> expected = nullptr,
                     shared_ptr<MatchMetadata> emulating = nullptr) noexcept;

  /// A command accesses content for an artifact and expects to find a particular version
  void matchContent(shared_ptr<Command> c,
                    shared_ptr<Ref> ref,
                    shared_ptr<Version> expected = nullptr,
                    shared_ptr<MatchContent> emulating = nullptr) noexcept;

  /// A command modifies the metadata for an artifact
  void updateMetadata(shared_ptr<Command> c,
                      shared_ptr<Ref>,
                      shared_ptr<MetadataVersion> written = nullptr,
                      shared_ptr<UpdateMetadata> emulating = nullptr) noexcept;

  /// A command writes a new version to an artifact
  void updateContent(shared_ptr<Command> c,
                     shared_ptr<Ref> ref,
                     shared_ptr<Version> written = nullptr,
                     shared_ptr<UpdateContent> emulating = nullptr) noexcept;

  /// Can a traced execveat skip a command with the given arguments?
  shared_ptr<Command> can_skip(shared_ptr<Access> exe_ref, vector<string> args) noexcept;

  /// The tracing layer is skipping a command, which is expected to run in the given process
  void skip_launch(shared_ptr<Command> c, shared_ptr<Process> proc) noexcept;

  /// A command is launching a child command
  void launch(shared_ptr<Command> c,
              shared_ptr<Command> child,
              shared_ptr<Launch> emulating = nullptr) noexcept;

  /// A command is joining with a child command
  void join(shared_ptr<Command> c,
            shared_ptr<Command> child,
            int exit_status,
            shared_ptr<Join> emulating = nullptr) noexcept;

  /// A command has exited with an exit code
  void exit(shared_ptr<Command> c, int exit_status, shared_ptr<Exit> emulating = nullptr) noexcept;

  /// Print information about this build
  ostream& print(ostream& o) const noexcept;

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
  void observeOutput(shared_ptr<Command> c, shared_ptr<Artifact> a, shared_ptr<Version> v) const
      noexcept;

  /// Inform observers that command c accessed version v of artifact a
  void observeInput(shared_ptr<Command> c,
                    shared_ptr<Artifact> a,
                    shared_ptr<Version> v,
                    InputType t) const noexcept;

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
  /// Run steps in this build until we hit the end of the provided trace
  void runSteps() noexcept;

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
};