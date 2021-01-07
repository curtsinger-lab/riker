#pragma once

#include <filesystem>
#include <fstream>
#include <map>
#include <memory>
#include <string>

#include <cereal/archives/binary.hpp>

#include "data/IRSink.hh"
#include "data/Record.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"

using std::map;
using std::ofstream;
using std::shared_ptr;
using std::string;
using std::unique_ptr;

namespace fs = std::filesystem;

class MetadataVersion;

/**
 * An output trace is used to write a trace to disk
 */
class OutputTrace : public IRSink {
 public:
  /// Create a trace at the given path
  OutputTrace(string filename) noexcept;

  // Disallow copy
  OutputTrace(const OutputTrace&) = delete;
  OutputTrace& operator=(const OutputTrace&) = delete;

  /// Add a new command to the output trace and return its unique ID
  Command::ID addCommand(const shared_ptr<Command>& cmd) noexcept {
    Command::ID id = _commands.size();
    _commands.emplace(cmd, id);
    return id;
  }

  /// Get the ID for a command instance
  Command::ID getCommandID(const shared_ptr<Command>& cmd) const noexcept {
    return _commands.at(cmd);
  }

  /// Trace output is finished
  virtual void finish() noexcept override;

  /// Add a SpecialRef IR step to the output trace
  virtual void specialRef(const shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept override;

  /// Add a PipeRef IR step to the output trace
  virtual void pipeRef(const shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override;

  /// Add a FileRef IR step to the output trace
  virtual void fileRef(const shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept override;

  /// Add a SymlinkRef IR step to the output trace
  virtual void symlinkRef(const shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept override;

  /// Add a DirRef IR step to the output trace
  virtual void dirRef(const shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept override;

  /// Add a PathRef IR step to the output trace
  virtual void pathRef(const shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override;

  /// Add a UsingRef IR step to the output trace
  virtual void usingRef(const shared_ptr<Command>& command, Ref::ID ref) noexcept override;

  /// Add a DoneWithRef IR step to the output trace
  virtual void doneWithRef(const shared_ptr<Command>& command, Ref::ID ref) noexcept override;

  /// Add a CompareRefs IR step to the output trace
  virtual void compareRefs(const shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override;

  /// Add a ExpectResult IR step to the output trace
  virtual void expectResult(const shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override;

  /// Add a MatchMetadata IR step to the output trace
  virtual void matchMetadata(const shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             shared_ptr<MetadataVersion> version) noexcept override;

  /// Add a MatchContent IR step to the output trace
  virtual void matchContent(const shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            shared_ptr<ContentVersion> version) noexcept override;

  /// Add a UpdateMetadata IR step to the output trace
  virtual void updateMetadata(const shared_ptr<Command>& command,
                              Ref::ID ref,
                              shared_ptr<MetadataVersion> version) noexcept override;

  /// Add a UpdateContent IR step to the output trace
  virtual void updateContent(const shared_ptr<Command>& command,
                             Ref::ID ref,
                             shared_ptr<ContentVersion> version) noexcept override;

  /// Add an AddEntry IR step to the output trace
  virtual void addEntry(const shared_ptr<Command>& command,
                        Ref::ID dir,
                        fs::path name,
                        Ref::ID target) noexcept override;

  /// Add a RemoveEntry IR step to the output trace
  virtual void removeEntry(const shared_ptr<Command>& command,
                           Ref::ID dir,
                           fs::path name,
                           Ref::ID target) noexcept override;

  /// Add a Launch IR step to the output trace
  virtual void launch(const shared_ptr<Command>& command,
                      const shared_ptr<Command>& child,
                      list<tuple<Ref::ID, Ref::ID>> refs) noexcept override;

  /// Add a Join IR step to the output trace
  virtual void join(const shared_ptr<Command>& command,
                    const shared_ptr<Command>& child,
                    int exit_status) noexcept override;

  /// Add a Exit IR step to the output trace
  virtual void exit(const shared_ptr<Command>& command, int exit_status) noexcept override;

 private:
  /// The output file stream
  ofstream _out;

  /// The cereal archive
  cereal::BinaryOutputArchive _archive;

  /// The map from commands to their IDs in the output trace
  map<shared_ptr<Command>, Command::ID> _commands;
};
