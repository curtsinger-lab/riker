#pragma once

#include <filesystem>
#include <fstream>
#include <list>
#include <memory>
#include <ostream>
#include <string>
#include <tuple>

#include <cereal/archives/binary.hpp>

#include "core/IR.hh"

using std::ifstream;
using std::list;
using std::ofstream;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::unique_ptr;

class Build;
class Command;
class Env;
class InputTrace;
class Step;

struct Record {
  Record() noexcept = default;
  virtual ~Record() = default;

  virtual void handleInput(InputTrace& input) noexcept = 0;

  template <class Archive>
  void serialize(Archive& archive) {}
};

/**
 * An input trace is a build trace loaded from disk
 */
class InputTrace {
 public:
  using StepList = list<tuple<shared_ptr<Command>, unique_ptr<Step>>>;

  /**
   * Load a trace from a given path. If the trace does not load, fall back to a default trace.
   */
  InputTrace(string filename) noexcept;

  // Disallow copy
  InputTrace(const InputTrace&) = delete;
  InputTrace& operator=(const InputTrace&) = delete;

  // Allow move
  InputTrace(InputTrace&&) = default;
  InputTrace& operator=(InputTrace&&) = default;

  /// Run this trace
  shared_ptr<Env> run(Build& build) noexcept;

  /// Print this trace
  ostream& print(ostream& o) const noexcept;

  friend ostream& operator<<(ostream& o, const InputTrace& t) noexcept { return t.print(o); }

  void addStep(shared_ptr<Command> command, unique_ptr<Step>&& step) {
    _steps.emplace_back(command, std::move(step));
  }

  void done() { _done = true; }

 private:
  /// Initialize the list of steps to a default trace
  void initDefault() noexcept;

 private:
  /// A sequence of tuples containing a command and a step performed by that command
  StepList _steps;

  bool _done = false;
};

/**
 * An output trace is used to write a trace to disk
 */
class OutputTrace {
 public:
  /// Create a trace at the given path
  OutputTrace(string filename) noexcept : _filename(filename) {}

  /// Finalize an output trace
  ~OutputTrace() noexcept;

  // Disallow copy
  OutputTrace(const OutputTrace&) = delete;
  OutputTrace& operator=(const OutputTrace&) = delete;

  /// Add a SpecialRef IR step to the output trace
  void specialRef(shared_ptr<Command> command,
                  SpecialRef::Entity entity,
                  shared_ptr<RefResult> output) noexcept;

  /// Add a PipeRef IR step to the output trace
  void pipeRef(shared_ptr<Command> command,
               shared_ptr<RefResult> read_end,
               shared_ptr<RefResult> write_end) noexcept;

  /// Add a FileRef IR step to the output trace
  void fileRef(shared_ptr<Command> command, mode_t mode, shared_ptr<RefResult> output) noexcept;

  /// Add a SymlinkRef IR step to the output trace
  void symlinkRef(shared_ptr<Command> command,
                  fs::path target,
                  shared_ptr<RefResult> output) noexcept;

  /// Add a DirRef IR step to the output trace
  void dirRef(shared_ptr<Command> command, mode_t mode, shared_ptr<RefResult> output) noexcept;

  /// Add a PathRef IR step to the output trace
  void pathRef(shared_ptr<Command> command,
               shared_ptr<RefResult> base,
               fs::path path,
               AccessFlags flags,
               shared_ptr<RefResult> output) noexcept;

  /// Add a ExpectResult IR step to the output trace
  void expectResult(shared_ptr<Command> command, shared_ptr<RefResult> ref, int expected) noexcept;

  /// Add a MatchMetadata IR step to the output trace
  void matchMetadata(shared_ptr<Command> command,
                     shared_ptr<RefResult> ref,
                     shared_ptr<MetadataVersion> version) noexcept;

  /// Add a MatchContent IR step to the output trace
  void matchContent(shared_ptr<Command> command,
                    shared_ptr<RefResult> ref,
                    shared_ptr<Version> version) noexcept;

  /// Add a UpdateMetadata IR step to the output trace
  void updateMetadata(shared_ptr<Command> command,
                      shared_ptr<RefResult> ref,
                      shared_ptr<MetadataVersion> version) noexcept;

  /// Add a UpdateContent IR step to the output trace
  void updateContent(shared_ptr<Command> command,
                     shared_ptr<RefResult> ref,
                     shared_ptr<Version> version) noexcept;

  /// Add a Launch IR step to the output trace
  void launch(shared_ptr<Command> command, shared_ptr<Command> child) noexcept;

  /// Add a Join IR step to the output trace
  void join(shared_ptr<Command> command, shared_ptr<Command> child, int exit_status) noexcept;

  /// Add a Exit IR step to the output trace
  void exit(shared_ptr<Command> command, int exit_status) noexcept;

 private:
  string _filename;
  list<unique_ptr<Record>> _records;
};