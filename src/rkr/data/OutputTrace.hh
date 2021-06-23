#pragma once

#include <filesystem>
#include <fstream>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>

#include <sys/types.h>

#include <cereal/archives/binary.hpp>

#include "data/IRSink.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

namespace fs = std::filesystem;

class AccessFlags;
class ContentVersion;
class MetadataVersion;

/**
 * An output trace is used to write a trace to disk
 */
class OutputTrace : public IRSink {
 public:
  /// Create a trace at the given path
  OutputTrace(std::string filename) noexcept;

  // Disallow copy
  OutputTrace(const OutputTrace&) = delete;
  OutputTrace& operator=(const OutputTrace&) = delete;

  /// Get the ID for a command instance. Emit a new command record if necessary
  Command::ID getCommandID(const std::shared_ptr<Command>& c) noexcept;

  /// Get the ID for a content version. Emit a new content version record if necessary
  ContentVersion::ID getContentVersionID(const std::shared_ptr<ContentVersion>& cv) noexcept;

  /// Trace output is starting
  virtual void start(const std::shared_ptr<Command>& root) noexcept override;

  /// Trace output is finished
  virtual void finish() noexcept override;

  /// Add a SpecialRef IR step to the output trace
  virtual void specialRef(const std::shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept override;

  /// Add a PipeRef IR step to the output trace
  virtual void pipeRef(const std::shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override;

  /// Add a FileRef IR step to the output trace
  virtual void fileRef(const std::shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept override;

  /// Add a SymlinkRef IR step to the output trace
  virtual void symlinkRef(const std::shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept override;

  /// Add a DirRef IR step to the output trace
  virtual void dirRef(const std::shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept override;

  /// Add a PathRef IR step to the output trace
  virtual void pathRef(const std::shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override;

  /// Add a UsingRef IR step to the output trace
  virtual void usingRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override;

  /// Add a DoneWithRef IR step to the output trace
  virtual void doneWithRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override;

  /// Add a CompareRefs IR step to the output trace
  virtual void compareRefs(const std::shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override;

  /// Add a ExpectResult IR step to the output trace
  virtual void expectResult(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override;

  /// Add a MatchMetadata IR step to the output trace
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion version) noexcept override;

  /// Add a MatchContent IR step to the output trace
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> version) noexcept override;

  /// Add a UpdateMetadata IR step to the output trace
  virtual void updateMetadata(const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              MetadataVersion version) noexcept override;

  /// Add a UpdateContent IR step to the output trace
  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> version) noexcept override;

  /// Add an AddEntry IR step to the output trace
  virtual void addEntry(const std::shared_ptr<Command>& command,
                        Ref::ID dir,
                        std::string name,
                        Ref::ID target) noexcept override;

  /// Add a RemoveEntry IR step to the output trace
  virtual void removeEntry(const std::shared_ptr<Command>& command,
                           Ref::ID dir,
                           std::string name,
                           Ref::ID target) noexcept override;

  /// Add a Launch IR step to the output trace
  virtual void launch(const std::shared_ptr<Command>& command,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept override;

  /// Add a Join IR step to the output trace
  virtual void join(const std::shared_ptr<Command>& command,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept override;

  /// Add a Exit IR step to the output trace
  virtual void exit(const std::shared_ptr<Command>& command, int exit_status) noexcept override;

 private:
  size_t _id;

  /// The output file stream
  std::ofstream _out;

  /// The cereal archive
  cereal::BinaryOutputArchive _archive;

  /// The map from commands to their IDs in the output trace
  std::unordered_map<std::shared_ptr<Command>, Command::ID> _commands;

  /// The map from content versions to their IDs in the output trace
  std::unordered_map<std::shared_ptr<ContentVersion>, ContentVersion::ID> _content_versions;
};
