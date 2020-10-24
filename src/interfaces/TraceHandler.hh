#pragma once

#include <filesystem>
#include <list>
#include <memory>
#include <tuple>

#include "data/AccessFlags.hh"
#include "runtime/Command.hh"

class Command;
class MetadataVersion;
class Ref;
class Version;

using std::list;
using std::shared_ptr;
using std::tuple;

namespace fs = std::filesystem;

/**
 * Predicates are tagged with specific scenarios where they apply.
 *
 * If all of a command's predicates in the Build scenario evaluate to true, the command does not
 * directly observe any change. The same is true for the PostBuild scenario.
 */
enum class Scenario { Build, PostBuild };

/// Unique IDs for the entities reachable via special references
enum class SpecialRef { stdin, stdout, stderr, root, cwd, launch_exe };

/// Different ways to compare references with a CompareRefs predicate
enum class RefComparison { SameInstance, DifferentInstances };

class TraceHandler {
 public:
  virtual ~TraceHandler() noexcept {}

  /// Called when the trace is finished
  virtual void finish() noexcept {}

  /// Handle a SpecialRef IR step
  virtual void specialRef(shared_ptr<Command> command,
                          SpecialRef entity,
                          Command::RefID output) noexcept {};

  /// Handle a PipeRef IR step
  virtual void pipeRef(shared_ptr<Command> command,
                       Command::RefID read_end,
                       Command::RefID write_end) noexcept {};

  /// Handle a FileRef IR step
  virtual void fileRef(shared_ptr<Command> command, mode_t mode, Command::RefID output) noexcept {};

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(shared_ptr<Command> command,
                          fs::path target,
                          Command::RefID output) noexcept {};

  /// Handle a DirRef IR step
  virtual void dirRef(shared_ptr<Command> command, mode_t mode, Command::RefID output) noexcept {};

  /// Handle a PathRef IR step
  virtual void pathRef(shared_ptr<Command> command,
                       Command::RefID base,
                       fs::path path,
                       AccessFlags flags,
                       Command::RefID output) noexcept {};

  /// Handle a UsingRef IR step
  virtual void usingRef(shared_ptr<Command> command, Command::RefID ref) noexcept {}

  /// Handle a DoneWithRef IR step
  virtual void doneWithRef(shared_ptr<Command> command, Command::RefID ref) noexcept {}

  /// Handle a CompareRefs IR step
  virtual void compareRefs(shared_ptr<Command> command,
                           Command::RefID ref1,
                           Command::RefID ref2,
                           RefComparison type) noexcept {};

  /// Handle an ExpectResult IR step
  virtual void expectResult(shared_ptr<Command> command,
                            Scenario scenario,
                            Command::RefID ref,
                            int expected) noexcept {};

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(shared_ptr<Command> command,
                             Scenario scenario,
                             Command::RefID ref,
                             shared_ptr<MetadataVersion> version) noexcept {};

  /// Handel a MatchContent IR step
  virtual void matchContent(shared_ptr<Command> command,
                            Scenario scenario,
                            Command::RefID ref,
                            shared_ptr<Version> version) noexcept {};

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(shared_ptr<Command> command,
                              Command::RefID ref,
                              shared_ptr<MetadataVersion> version) noexcept {};

  /// Handle an UpdateContent IR step
  virtual void updateContent(shared_ptr<Command> command,
                             Command::RefID ref,
                             shared_ptr<Version> version) noexcept {};

  /// Handle an AddEntry IR step
  virtual void addEntry(shared_ptr<Command> command,
                        Command::RefID dir,
                        fs::path name,
                        Command::RefID target) noexcept {};

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(shared_ptr<Command> command,
                           Command::RefID dir,
                           fs::path name,
                           Command::RefID target) noexcept {};

  /// Handle a Launch IR step
  virtual void launch(shared_ptr<Command> command,
                      shared_ptr<Command> child,
                      list<tuple<Command::RefID, Command::RefID>> refs) noexcept {};

  /// Handle a Join IR step
  virtual void join(shared_ptr<Command> command,
                    shared_ptr<Command> child,
                    int exit_status) noexcept {};

  /// Handle an Exit IR step
  virtual void exit(shared_ptr<Command> command, int exit_status) noexcept {};
};
