#pragma once

#include <list>
#include <memory>
#include <ostream>
#include <set>
#include <tuple>
#include <vector>

#include "data/InputTrace.hh"
#include "data/OutputTrace.hh"
#include "interfaces/BuildObserver.hh"
#include "interfaces/TraceHandler.hh"
#include "runtime/Command.hh"
#include "runtime/Env.hh"
#include "runtime/Ref.hh"
#include "tracing/Tracer.hh"

using std::list;
using std::make_shared;
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
class Build : public TraceHandler, public BuildObserver {
 private:
  /// Create a build runner
  Build(bool commit, BuildObserver& observer, TraceHandler& output, fs::path cache_dir) noexcept :
      _commit(commit),
      _observer(observer),
      _output(output),
      _env(make_shared<Env>()),
      _tracer(*this),
      _cache_dir(cache_dir) {}

 public:
  /// Create a build runner that exclusively emulates trace steps
  static Build emulate(fs::path cache_dir,
                       BuildObserver& observer = _default_observer,
                       TraceHandler& output = _default_output) noexcept {
    return Build(false, observer, output, cache_dir);
  }

  static Build emulate(fs::path cache_dir, TraceHandler& output) noexcept {
    return Build(false, _default_observer, output, cache_dir);
  }

  /// Create a build runner that executes a rebuild plan
  static Build rebuild(fs::path cache_dir, TraceHandler& output = _default_output) noexcept {
    return Build(true, _default_observer, output, cache_dir);
  }

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /// Get the environment used in this build
  shared_ptr<Env> getEnvironment() const noexcept { return _env; }

  /// Get the number of steps this build has executed
  size_t getStepCount() const noexcept { return _emulated_step_count + _traced_step_count; }

  /// Get the number of steps this build emulated
  size_t getEmulatedStepCount() const noexcept { return _emulated_step_count; }

  /// Get the number of steps this build traced
  size_t getTracedStepCount() const noexcept { return _traced_step_count; }

  /// Get the number of commands this build has executed
  size_t getCommandCount() const noexcept {
    return _emulated_command_count + _traced_command_count;
  }

  /// Get the number of commands this build emulated
  size_t getEmulatedCommandCount() const noexcept { return _emulated_command_count; }

  /// Get the number of commands this build traced
  size_t getTracedCommandCount() const noexcept { return _traced_command_count; }

  /// Get the number of syscalls handled by this build
  size_t getSyscallCount() const noexcept { return _num_syscalls; }

  /// Get the number of ptrace stops recorded in this build
  size_t getPTraceStopCount() const noexcept { return _num_ptrace_stops; }

  /// Print information about this build
  ostream& print(ostream& o) const noexcept;

  /// Increments a syscall count
  void countSyscall() { _num_syscalls++; }

  /// Increments a ptrace stop count
  void countPTraceStop() { _num_ptrace_stops++; }

  /********** Handle IR steps supplied from a loaded trace **********/

  /// A command is issuing a reference to a special artifact (e.g. stdin, stdout, root dir)
  virtual void specialRef(const shared_ptr<Command>& c,
                          SpecialRef entity,
                          Ref::ID output) noexcept override;

  /// A command references a new anonymous pipe
  virtual void pipeRef(const shared_ptr<Command>& c,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override;

  /// A command references a new anonymous file
  virtual void fileRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept override;

  /// A command references a new anonymous symlink
  virtual void symlinkRef(const shared_ptr<Command>& c,
                          fs::path target,
                          Ref::ID output) noexcept override;

  /// A command references a new anonymous directory
  virtual void dirRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept override;

  /// A command makes a reference with a path
  virtual void pathRef(const shared_ptr<Command>& c,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override;

  /// A command retains a handle to a Ref
  virtual void usingRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept override;

  /// A command is finished with a specific Ref
  virtual void doneWithRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept override;

  /// A command depends on the outcome of comparing two different references
  virtual void compareRefs(const shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override;

  /// A command expects a reference to resolve with a particular result
  virtual void expectResult(const shared_ptr<Command>& c,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override;

  /// A command accesses metadata for an artifact and expects to find a particular version
  virtual void matchMetadata(const shared_ptr<Command>& c,
                             Scenario scenario,
                             Ref::ID ref,
                             shared_ptr<MetadataVersion> expected) noexcept override;

  /// A command accesses content for an artifact and expects to find a particular version
  virtual void matchContent(const shared_ptr<Command>& c,
                            Scenario scenario,
                            Ref::ID ref,
                            shared_ptr<Version> expected) noexcept override;

  /// A command modifies the metadata for an artifact
  virtual void updateMetadata(const shared_ptr<Command>& c,
                              Ref::ID,
                              shared_ptr<MetadataVersion> written) noexcept override;

  /// A command writes a new version to an artifact
  virtual void updateContent(const shared_ptr<Command>& c,
                             Ref::ID ref,
                             shared_ptr<Version> written) noexcept override;

  /// A command adds an entry to a directory
  virtual void addEntry(const shared_ptr<Command>& command,
                        Ref::ID dir,
                        fs::path name,
                        Ref::ID target) noexcept override;

  /// A command removes an entry from a directory
  virtual void removeEntry(const shared_ptr<Command>& command,
                           Ref::ID dir,
                           fs::path name,
                           Ref::ID target) noexcept override;

  /**
   * An emulated command is launching a child command
   *
   * \param c     The parent command
   * \param child The child command
   * \param refs  A list of reference mappings. The first entry is a reference ID in the parent
   *              command, and the second is the ID where this reference is assigned the child.
   */
  virtual void launch(const shared_ptr<Command>& c,
                      const shared_ptr<Command>& child,
                      list<tuple<Ref::ID, Ref::ID>> refs) noexcept override;

  /// A command is joining with a child command
  virtual void join(const shared_ptr<Command>& c,
                    const shared_ptr<Command>& child,
                    int exit_status) noexcept override;

  /// A command has exited with an exit code
  virtual void exit(const shared_ptr<Command>& c, int exit_status) noexcept override;

  /// Finish running an emulated build
  virtual void finish() noexcept override;

  /********** Handle IR steps delivered from the tracing layer **********/

  /// A traced command referenced a new anonymous pipe
  tuple<Ref::ID, Ref::ID> tracePipeRef(const shared_ptr<Command>& c) noexcept;

  /// A traced command referenced a new anonymous file
  Ref::ID traceFileRef(const shared_ptr<Command>& c, mode_t mode) noexcept;

  /// A traced command referenced a new anonymous symlink
  Ref::ID traceSymlinkRef(const shared_ptr<Command>& c, fs::path target) noexcept;

  /// A traced command referenced a new anonymous directory
  Ref::ID traceDirRef(const shared_ptr<Command>& c, mode_t mode) noexcept;

  /// A traced command referenced a path
  Ref::ID tracePathRef(const shared_ptr<Command>& c,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags) noexcept;

  /// A command is retaining a handle to a Ref (e.g. in its file descriptor table)
  void traceUsingRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept;

  /// A command has closed a handle to a Ref
  void traceDoneWithRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept;

  /// A command compares two references and expects a specific result
  void traceCompareRefs(const shared_ptr<Command>& c,
                        Ref::ID ref1,
                        Ref::ID ref2,
                        RefComparison type) noexcept;

  /// A command expects a reference to resolve with a particular result
  void traceExpectResult(const shared_ptr<Command>& c, Ref::ID ref, int expected = -1) noexcept;

  /// A command accesses metadata for an artifact and expects to find a particular version
  void traceMatchMetadata(const shared_ptr<Command>& c, Ref::ID ref) noexcept;

  /// A command accesses content for an artifact and expects to find a particular version
  void traceMatchContent(const shared_ptr<Command>& c,
                         Ref::ID ref,
                         shared_ptr<Version> expected) noexcept;

  /// A command modifies the metadata for an artifact
  void traceUpdateMetadata(const shared_ptr<Command>& c, Ref::ID ref) noexcept;

  /// A command writes a new version to an artifact
  void traceUpdateContent(const shared_ptr<Command>& c,
                          Ref::ID ref,
                          shared_ptr<Version> written) noexcept;

  /// Handle an AddEntry IR step
  void traceAddEntry(const shared_ptr<Command>& command,
                     Ref::ID dir,
                     fs::path name,
                     Ref::ID target) noexcept;

  /// Handle a RemoveEntry IR step
  void traceRemoveEntry(const shared_ptr<Command>& command,
                        Ref::ID dir,
                        fs::path name,
                        Ref::ID target) noexcept;

  /**
   * A traced command is launching a child command.
   *
   * \param c         The parent command
   * \param args      The command line arguments passed to the child command
   * \param exe_ref   The parent command's reference to the launched executable
   * \param cwd_ref   The parent command's reference to the working directory
   * \param root_ref  The parent command's reference to the root directory
   * \param fds       A mapping from child file descriptor numbers to the parent's reference
   * \returns The child command that has been launched
   */
  shared_ptr<Command> traceLaunch(const shared_ptr<Command>& c,
                                  vector<string> args,
                                  Ref::ID exe_ref,
                                  Ref::ID cwd_ref,
                                  Ref::ID root_ref,
                                  map<int, Ref::ID> fds) noexcept;

  /// A command is joining with a child command
  void traceJoin(const shared_ptr<Command>& c,
                 const shared_ptr<Command>& child,
                 int exit_status) noexcept;

  /// A command has exited with an exit code
  void traceExit(const shared_ptr<Command>& c, int exit_status) noexcept;

  /********** Observer Interface **********/

  /// Inform observers that a command has never run
  virtual void observeCommandNeverRun(const shared_ptr<Command>& c) noexcept override;

  /// Inform observers that a parent command launched a child command
  virtual void observeLaunch(const shared_ptr<Command>& parent,
                             const shared_ptr<Command>& child) noexcept override;

  /// Inform observers that command c modified artifact a, creating version v
  virtual void observeOutput(const shared_ptr<Command>& c,
                             shared_ptr<Artifact> a,
                             shared_ptr<Version> v) noexcept override;

  /// Inform observers that command c accessed version v of artifact a
  virtual void observeInput(const shared_ptr<Command>& c,
                            shared_ptr<Artifact> a,
                            shared_ptr<Version> v,
                            InputType t) noexcept override;

  /// Inform observers that command c did not find the expected version in artifact a
  /// Instead of version `expected`, the command found version `observed`
  virtual void observeMismatch(const shared_ptr<Command>& c,
                               Scenario scenario,
                               shared_ptr<Artifact> a,
                               shared_ptr<Version> observed,
                               shared_ptr<Version> expected) noexcept override;

  /// Inform observers that the version of an artifact produced during the build does not match
  /// the on-disk version.
  virtual void observeFinalMismatch(shared_ptr<Artifact> a,
                                    shared_ptr<Version> produced,
                                    shared_ptr<Version> ondisk) noexcept override;

  /// Inform observers that two references did not compare as expected
  virtual void observeRefMismatch(const shared_ptr<Command>& c,
                                  shared_ptr<Ref> ref1,
                                  shared_ptr<Ref> ref2,
                                  RefComparison type) noexcept override;

  /// Inform observers that a command's exit code changed
  virtual void observeExitCodeChange(const shared_ptr<Command>& parent,
                                     const shared_ptr<Command>& child,
                                     int expected,
                                     int observed) noexcept override;

 private:
  /// Is a particular command running?
  bool isRunning(const shared_ptr<Command>& c) const noexcept {
    return _running.find(c) != _running.end();
  }

 private:
  /// The number of syscalls handled in this build
  size_t _num_syscalls = 0;

  /// The number of ptrace stops in this build
  size_t _num_ptrace_stops = 0;

  /// The number of IR steps emulated in this build
  size_t _emulated_step_count = 0;

  /// The number of IR steps traced in this build
  size_t _traced_step_count = 0;

  /// The count of commands emulated in this build
  size_t _emulated_command_count = 0;

  /// The count of commands traced in this build
  size_t _traced_command_count = 0;

  /// Should this build commit the environment to the filesystem when it's finished?
  bool _commit;

  /// The observers that should be notified of dependency and change information during the build
  BuildObserver& _observer;

  /// Trace steps are sent to this trace handler, typically an OutputTrace
  TraceHandler& _output;

  /// The environment in which this build executes
  shared_ptr<Env> _env;

  /// The set of commands that were run by this build (both traced and emulated commands included)
  set<shared_ptr<Command>> _commands;

  /// The tracer that will be used to execute any commands that must rerun
  Tracer _tracer;

  /// A map of launched commands to the root process running that command, or nullptr if it is
  /// only being emulated
  map<shared_ptr<Command>, shared_ptr<Process>> _running;

  /// The default observer is used if an observer is not provided during setup
  inline static BuildObserver _default_observer;

  /// The default output is used if a trace handler is not provided during setup
  inline static TraceHandler _default_output;

  /// The path to the file cache
  fs::path _cache_dir;
};