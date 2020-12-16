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
  virtual void specialRef(const shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept {};

  /// Handle a PipeRef IR step
  virtual void pipeRef(const shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept {};

  /// Handle a FileRef IR step
  virtual void fileRef(const shared_ptr<Command>& command, mode_t mode, Ref::ID output) noexcept {};

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(const shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept {};

  /// Handle a DirRef IR step
  virtual void dirRef(const shared_ptr<Command>& command, mode_t mode, Ref::ID output) noexcept {};

  /// Handle a PathRef IR step
  virtual void pathRef(const shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept {};

  /// Handle a UsingRef IR step
  virtual void usingRef(const shared_ptr<Command>& command, Ref::ID ref) noexcept {}

  /// Handle a DoneWithRef IR step
  virtual void doneWithRef(const shared_ptr<Command>& command, Ref::ID ref) noexcept {}

  /// Handle a CompareRefs IR step
  virtual void compareRefs(const shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept {};

  /// Handle an ExpectResult IR step
  virtual void expectResult(const shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept {};

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             shared_ptr<MetadataVersion> version) noexcept {};

  /// Handel a MatchContent IR step
  virtual void matchContent(const shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            shared_ptr<Version> version) noexcept {};

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(const shared_ptr<Command>& command,
                              Ref::ID ref,
                              shared_ptr<MetadataVersion> version) noexcept {};

  /// Handle an UpdateContent IR step
  virtual void updateContent(const shared_ptr<Command>& command,
                             Ref::ID ref,
                             shared_ptr<Version> version) noexcept {};

  /// Handle an AddEntry IR step
  virtual void addEntry(const shared_ptr<Command>& command,
                        Ref::ID dir,
                        fs::path name,
                        Ref::ID target) noexcept {};

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const shared_ptr<Command>& command,
                           Ref::ID dir,
                           fs::path name,
                           Ref::ID target) noexcept {};

  /// Handle a Launch IR step
  virtual void launch(const shared_ptr<Command>& command,
                      const shared_ptr<Command>& child,
                      list<tuple<Ref::ID, Ref::ID>> refs) noexcept {};

  /// Handle a Join IR step
  virtual void join(const shared_ptr<Command>& command,
                    const shared_ptr<Command>& child,
                    int exit_status) noexcept {};

  /// Handle an Exit IR step
  virtual void exit(const shared_ptr<Command>& command, int exit_status) noexcept {};
};
