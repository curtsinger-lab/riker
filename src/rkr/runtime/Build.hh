#pragma once

#include <filesystem>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <tuple>
#include <vector>

#include <sys/types.h>

#include "data/IRSink.hh"
#include "data/Trace.hh"
#include "runtime/Ref.hh"
#include "tracing/Tracer.hh"

namespace fs = std::filesystem;

class AccessFlags;
class Command;
class ContentVersion;
class MetadataVersion;
class Process;

/**
 * A Build instance manages the execution of a build. This instance is responsible for setting up
 * the build environment, emulating or running each of the commands, and concluding the build.
 */
class Build : public IRSink {
 public:
  /// Create a build runner
  Build(IRSink& output, std::ostream& print_to = std::cout) noexcept;

  /// Create a build runner that uses the default output IRSink
  Build(std::ostream& print_to = std::cout) noexcept : Build(_default_output, print_to) {}

  // Disallow Copy
  Build(const Build&) = delete;
  Build& operator=(const Build&) = delete;

  /// Try to run any steps that were deferred because they came from commands that had not launched
  void runDeferredSteps() noexcept;

  /// Print information about this build
  std::ostream& print(std::ostream& o) const noexcept;

  /********** Handle IR steps supplied from a loaded trace **********/

  /// Start a build with the given root command
  virtual void start(const std::shared_ptr<Command>& c) noexcept override;

  /// A command is issuing a reference to a special artifact (e.g. stdin, stdout, root dir)
  virtual void specialRef(const std::shared_ptr<Command>& c,
                          SpecialRef entity,
                          Ref::ID output) noexcept override;

  /// A command references a new anonymous pipe
  virtual void pipeRef(const std::shared_ptr<Command>& c,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override;

  /// A command references a new anonymous file
  virtual void fileRef(const std::shared_ptr<Command>& c,
                       mode_t mode,
                       Ref::ID output) noexcept override;

  /// A command references a new anonymous symlink
  virtual void symlinkRef(const std::shared_ptr<Command>& c,
                          fs::path target,
                          Ref::ID output) noexcept override;

  /// A command references a new anonymous directory
  virtual void dirRef(const std::shared_ptr<Command>& c,
                      mode_t mode,
                      Ref::ID output) noexcept override;

  /// A command makes a reference with a path
  virtual void pathRef(const std::shared_ptr<Command>& c,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override;

  /// A command retains a handle to a Ref
  virtual void usingRef(const std::shared_ptr<Command>& c, Ref::ID ref) noexcept override;

  /// A command is finished with a specific Ref
  virtual void doneWithRef(const std::shared_ptr<Command>& c, Ref::ID ref) noexcept override;

  /// A command depends on the outcome of comparing two different references
  virtual void compareRefs(const std::shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override;

  /// A command expects a reference to resolve with a particular result
  virtual void expectResult(const std::shared_ptr<Command>& c,
                            Scenario scenario,
                            Ref::ID ref,
                            int8_t expected) noexcept override;

  /// A command accesses metadata for an artifact and expects to find a particular version
  virtual void matchMetadata(const std::shared_ptr<Command>& c,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion expected) noexcept override;

  /// A command accesses content for an artifact and expects to find a particular version
  virtual void matchContent(const std::shared_ptr<Command>& c,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> expected) noexcept override;

  /// A command modifies the metadata for an artifact
  virtual void updateMetadata(const std::shared_ptr<Command>& c,
                              Ref::ID,
                              MetadataVersion written) noexcept override;

  /// A command writes a new version to an artifact
  virtual void updateContent(const std::shared_ptr<Command>& c,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> written) noexcept override;

  /// A command adds an entry to a directory
  virtual void addEntry(const std::shared_ptr<Command>& command,
                        Ref::ID dir,
                        std::string name,
                        Ref::ID target) noexcept override;

  /// A command removes an entry from a directory
  virtual void removeEntry(const std::shared_ptr<Command>& command,
                           Ref::ID dir,
                           std::string name,
                           Ref::ID target) noexcept override;

  /// A parent command is launching a child command
  virtual void launch(const std::shared_ptr<Command>& parent,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept override;

  /// A parent command displays its orphans
  virtual void orphan(const std::shared_ptr<Command>& parent,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept override;                    

  /// A command is joining with a child command
  virtual void join(const std::shared_ptr<Command>& c,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept override;

  /// A command has exited with an exit code
  virtual void exit(const std::shared_ptr<Command>& c, int exit_status) noexcept override;

  /// Finish running a build
  virtual void finish() noexcept override;

  /// Look for a known command that matches one being launched
  std::shared_ptr<Command> findCommand(const std::shared_ptr<Command>& parent,
                                       std::vector<std::string> args,
                                       const std::map<int, Ref::ID>& fds) noexcept;

 private:
  /// Trace steps are sent to this trace handler, typically an OutputTrace
  IRSink& _output;

  /// Deferred trace steps are placed in this buffer for later running
  TraceWriter _deferred_steps;

  /// The set of deferred commands
  std::set<std::shared_ptr<Command>> _deferred_commands;

  /// The root command provided to this Build
  std::shared_ptr<Command> _root_command;

  /// The tracer that will be used to execute any commands that must rerun
  Tracer _tracer;

  /// The default output is used if a trace handler is not provided during setup
  inline static IRSink _default_output;

  /// The stream where commands should be printed, if at all
  std::ostream& _print_to;
};