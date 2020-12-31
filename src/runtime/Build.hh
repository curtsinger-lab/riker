#pragma once

#include <list>
#include <memory>
#include <ostream>
#include <set>
#include <tuple>
#include <vector>

#include "data/IRSink.hh"
#include "data/InputTrace.hh"
#include "data/OutputTrace.hh"
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
 * the build environment, emulating or running each of the commands, and concluding the build.
 */
class Build : public IRSink {
 public:
  /// Create a build runner
  Build(bool execute, shared_ptr<Env> env, IRSink& output = _default_output) noexcept :
      _execute(execute), _env(env), _output(output), _tracer(*this) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /// Get the list of commands in this build
  const set<shared_ptr<Command>>& getCommands() const noexcept { return _commands; }

  /// Print information about this build
  ostream& print(ostream& o) const noexcept;

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

 private:
  /// Should a step from the given command be emulated?
  bool canEmulate(const shared_ptr<Command>& c) noexcept;

  /// Is a particular command running?
  bool isRunning(const shared_ptr<Command>& c) const noexcept {
    return _running.find(c) != _running.end();
  }

 private:
  /// Is this build allowed to execute commands?
  bool _execute;

  /// The environment in which this build executes
  shared_ptr<Env> _env;

  /// Trace steps are sent to this trace handler, typically an OutputTrace
  IRSink& _output;

  /// The set of commands that were run by this build (both traced and emulated commands included)
  set<shared_ptr<Command>> _commands;

  /// The tracer that will be used to execute any commands that must rerun
  Tracer _tracer;

  /// A map of launched commands to the root process running that command, or nullptr if it is
  /// only being emulated
  map<shared_ptr<Command>, shared_ptr<Process>> _running;

  /// The default output is used if a trace handler is not provided during setup
  inline static IRSink _default_output;
};