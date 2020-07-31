#pragma once

#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <tuple>
#include <vector>

#include "build/BuildObserver.hh"
#include "build/Env.hh"
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
  Build() noexcept : _tracer(*this) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /// Mark command c for re-execution rather than emulation
  bool setRerun(shared_ptr<Command> c) noexcept {
    auto [iter, added] = _rerun.insert(c);
    return added;
  }

  /// Check if command c is marked for re-execution rather than emulation
  bool checkRerun(shared_ptr<Command> c) const noexcept { return _rerun.find(c) != _rerun.end(); }

  /**
   * Run a build trace in a given environment.
   * \param trace The trace to run
   * \param env   The environment the build should execute in. If no environment is provided, the
   *              trace will be run against a default environment (the current filesystem state).
   *              The provided environment will be modified by this method.
   * \returns a tuple of the new traces produced by the run, and the environment in its final state
   */
  tuple<shared_ptr<Trace>, shared_ptr<Env>> run(shared_ptr<Trace> trace,
                                                shared_ptr<Env> env = nullptr) noexcept;

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
  void applyMetadata(shared_ptr<Command> c,
                     shared_ptr<Ref>,
                     shared_ptr<MetadataVersion> written = nullptr,
                     shared_ptr<ApplyMetadata> emulating = nullptr) noexcept;

  /// A command writes a new version to an artifact
  template <class VersionType>
  void apply(shared_ptr<Command> c,
             shared_ptr<Ref> ref,
             shared_ptr<VersionType> written,
             shared_ptr<Apply<VersionType>> emulating = nullptr) noexcept;

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

  friend ostream& operator<<(ostream& o, const Build& b) noexcept { return b.print(o); }

  friend ostream& operator<<(ostream& o, const Build* b) noexcept {
    if (b == nullptr) return o << "<null Build>";
    return b->print(o);
  }

  /********** Observer Interface **********/

  /// Add an observer to this build
  void addObserver(shared_ptr<BuildObserver> o) noexcept { _observers.push_back(o); }

  /// Inform the observer that command c modified artifact a, creating version v
  void observeOutput(shared_ptr<Command> c,
                     shared_ptr<Artifact> a,
                     shared_ptr<Version> v) noexcept {
    for (const auto& o : _observers) o->output(c, a, v);
  }

  /// Inform the observer that command c accessed version v of artifact a
  void observeInput(shared_ptr<Command> c,
                    shared_ptr<Artifact> a,
                    shared_ptr<Version> v,
                    InputType t) noexcept;

  /// Inform the observer that command c did not find the expected version in artifact a
  /// Instead of version `expected`, the command found version `observed`
  void observeMismatch(shared_ptr<Command> c,
                       shared_ptr<Artifact> a,
                       shared_ptr<Version> observed,
                       shared_ptr<Version> expected) noexcept {
    for (const auto& o : _observers) o->mismatch(c, a, observed, expected);
  }

  /// Inform the observer that a given command's IR action would detect a change in the build env
  void observeCommandChange(shared_ptr<Command> c, shared_ptr<const Step> s) noexcept {
    for (const auto& o : _observers) o->commandChanged(c, s);
  }

  /// Inform observers that the version of an artifact produced during the build does not match the
  /// on-disk version.
  void observeFinalMismatch(shared_ptr<Artifact> a,
                            shared_ptr<Version> produced,
                            shared_ptr<Version> ondisk) noexcept {
    for (const auto& o : _observers) o->finalMismatch(a, produced, ondisk);
  }

 private:
  /// The trace of steps in this build
  shared_ptr<Trace> _trace;

  /// The environment in which this build executes
  shared_ptr<Env> _env;

  /// The tracer that will be used to execute any commands that must rerun
  Tracer _tracer;

  /// The set of commands that will be rerun, rather than emulated, by this build
  set<shared_ptr<Command>> _rerun;

  /// A map of running commands to their root processes
  map<shared_ptr<Command>, shared_ptr<Process>> _running;

  /// The observers that should be notified of dependency and change information during the build
  vector<shared_ptr<BuildObserver>> _observers;
};