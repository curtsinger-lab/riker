#pragma once

#include <filesystem>
#include <list>
#include <map>
#include <memory>
#include <string>

#include "data/Record.hh"
#include "interfaces/TraceHandler.hh"
#include "runtime/Command.hh"
#include "runtime/RefResult.hh"

using std::list;
using std::map;
using std::shared_ptr;
using std::string;
using std::unique_ptr;

namespace fs = std::filesystem;

/**
 * An output trace is used to write a trace to disk
 */
class OutputTrace : public TraceHandler {
 public:
  /// Create a trace at the given path
  OutputTrace(string filename) noexcept : _filename(filename) {}

  // Disallow copy
  OutputTrace(const OutputTrace&) = delete;
  OutputTrace& operator=(const OutputTrace&) = delete;

  /// Add a new command to the output trace and return its unique ID
  Command::ID addCommand(shared_ptr<Command> cmd) noexcept {
    Command::ID id = _commands.size();
    _commands.emplace(cmd, id);
    return id;
  }

  /// Get the ID for a command instance
  Command::ID getCommandID(shared_ptr<Command> cmd) const noexcept { return _commands.at(cmd); }

  /// Get the ID for a RefResult instance
  RefResult::ID getRefResultID(shared_ptr<RefResult> result) noexcept {
    auto iter = _ref_results.find(result);
    if (iter == _ref_results.end()) {
      RefResult::ID id = _ref_results.size();
      _ref_results.emplace_hint(iter, result, id);
      return id;
    } else {
      return iter->second;
    }
  }

  /// Trace output is finished
  virtual void finish() noexcept override;

  /// Add a SpecialRef IR step to the output trace
  virtual void specialRef(shared_ptr<Command> command,
                          SpecialRef entity,
                          shared_ptr<RefResult> output) noexcept override;

  /// Add a PipeRef IR step to the output trace
  virtual void pipeRef(shared_ptr<Command> command,
                       shared_ptr<RefResult> read_end,
                       shared_ptr<RefResult> write_end) noexcept override;

  /// Add a FileRef IR step to the output trace
  virtual void fileRef(shared_ptr<Command> command,
                       mode_t mode,
                       shared_ptr<RefResult> output) noexcept override;

  /// Add a SymlinkRef IR step to the output trace
  virtual void symlinkRef(shared_ptr<Command> command,
                          fs::path target,
                          shared_ptr<RefResult> output) noexcept override;

  /// Add a DirRef IR step to the output trace
  virtual void dirRef(shared_ptr<Command> command,
                      mode_t mode,
                      shared_ptr<RefResult> output) noexcept override;

  /// Add a PathRef IR step to the output trace
  virtual void pathRef(shared_ptr<Command> command,
                       shared_ptr<RefResult> base,
                       fs::path path,
                       AccessFlags flags,
                       shared_ptr<RefResult> output) noexcept override;

  /// Add a ExpectResult IR step to the output trace
  virtual void expectResult(shared_ptr<Command> command,
                            shared_ptr<RefResult> ref,
                            int expected) noexcept override;

  /// Add a MatchMetadata IR step to the output trace
  virtual void matchMetadata(shared_ptr<Command> command,
                             shared_ptr<RefResult> ref,
                             shared_ptr<MetadataVersion> version) noexcept override;

  /// Add a MatchContent IR step to the output trace
  virtual void matchContent(shared_ptr<Command> command,
                            shared_ptr<RefResult> ref,
                            shared_ptr<Version> version) noexcept override;

  /// Add a UpdateMetadata IR step to the output trace
  virtual void updateMetadata(shared_ptr<Command> command,
                              shared_ptr<RefResult> ref,
                              shared_ptr<MetadataVersion> version) noexcept override;

  /// Add a UpdateContent IR step to the output trace
  virtual void updateContent(shared_ptr<Command> command,
                             shared_ptr<RefResult> ref,
                             shared_ptr<Version> version) noexcept override;

  /// Add an AddEntry IR step to the output trace
  virtual void addEntry(shared_ptr<Command> command,
                        shared_ptr<RefResult> dir,
                        fs::path name,
                        shared_ptr<RefResult> target) noexcept override;

  /// Add a RemoveEntry IR step to the output trace
  virtual void removeEntry(shared_ptr<Command> command,
                           shared_ptr<RefResult> dir,
                           fs::path name,
                           shared_ptr<RefResult> target) noexcept override;

  /// Add a Launch IR step to the output trace
  virtual void launch(shared_ptr<Command> command, shared_ptr<Command> child) noexcept override;

  /// Add a Join IR step to the output trace
  virtual void join(shared_ptr<Command> command,
                    shared_ptr<Command> child,
                    int exit_status) noexcept override;

  /// Add a Exit IR step to the output trace
  virtual void exit(shared_ptr<Command> command, int exit_status) noexcept override;

 private:
  /// The path where this trace will be written
  string _filename;

  /// The list of records to write
  list<unique_ptr<Record>> _records;

  /// The map from commands to their IDs in the output trace
  map<shared_ptr<Command>, Command::ID> _commands = {{nullptr, 0}};

  /// The map from RefResults to their IDs in the output trace
  map<shared_ptr<RefResult>, RefResult::ID> _ref_results;
};
