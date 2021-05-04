#pragma once

#include <filesystem>
#include <fstream>
#include <functional>
#include <list>
#include <memory>
#include <string>
#include <unordered_map>

#include <cereal/archives/binary.hpp>

#include "data/IRLoader.hh"
#include "data/IRSink.hh"
#include "data/IRSource.hh"
#include "data/Record.hh"
#include "runtime/Command.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

class Command;
class MetadataVersion;

namespace fs = std::filesystem;

class IRBuffer : public IRSource, public IRSink, public IRLoader {
  enum class Mode { Filling, Draining, Drained };

 public:
  IRBuffer() noexcept;

  virtual ~IRBuffer() noexcept;

  /**** IRSource Methods ****/

  /// Send the stored IR trace to a sink
  virtual void sendTo(IRSink& handler) noexcept override;

  /**** IRSink Methods ****/

  /// Start a build with the given root command
  virtual void start(const std::shared_ptr<Command>& c) noexcept override {
    ASSERT(c) << "IRBuffer::start() called with null root command";
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(StartRecord::create(getCommandID(c)));
    _steps++;
  }

  /// Called when the trace is finished
  virtual void finish() noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(FinishRecord::create());
    _steps++;
  }

  /// Handle a SpecialRef IR step
  virtual void specialRef(const std::shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(SpecialRefRecord::create(getCommandID(command), entity, output));
    _steps++;
  }

  /// Handle a PipeRef IR step
  virtual void pipeRef(const std::shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(PipeRefRecord::create(getCommandID(command), read_end, write_end));
    _steps++;
  }

  /// Handle a FileRef IR step
  virtual void fileRef(const std::shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(FileRefRecord::create(getCommandID(command), mode, output));
    _steps++;
  }

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(const std::shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(SymlinkRefRecord::create(getCommandID(command), target, output));
    _steps++;
  }

  /// Handle a DirRef IR step
  virtual void dirRef(const std::shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(DirRefRecord::create(getCommandID(command), mode, output));
    _steps++;
  }

  /// Handle a PathRef IR step
  virtual void pathRef(const std::shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(PathRefRecord::create(getCommandID(command), base, path, flags, output));
    _steps++;
  }

  /// Handle a UsingRef IR step
  virtual void usingRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(UsingRefRecord::create(getCommandID(command), ref));
    _steps++;
  }

  /// Handle a DoneWithRef IR step
  virtual void doneWithRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(DoneWithRefRecord::create(getCommandID(command), ref));
    _steps++;
  }

  /// Handle a CompareRefs IR step
  virtual void compareRefs(const std::shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(CompareRefsRecord::create(getCommandID(command), ref1, ref2, type));
    _steps++;
  }

  /// Handle an ExpectResult IR step
  virtual void expectResult(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int expected) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(ExpectResultRecord::create(getCommandID(command), scenario, ref, expected));
    _steps++;
  }

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             std::shared_ptr<MetadataVersion> version) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(MatchMetadataRecord::create(getCommandID(command), scenario, ref,
                                         getMetadataVersionID(version)));
    _steps++;
  }

  /// Handel a MatchContent IR step
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> version) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(MatchContentRecord::create(getCommandID(command), scenario, ref,
                                        getContentVersionID(version)));
    _steps++;
  }

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              std::shared_ptr<MetadataVersion> version) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(
        UpdateMetadataRecord::create(getCommandID(command), ref, getMetadataVersionID(version)));
    _steps++;
  }

  /// Handle an UpdateContent IR step
  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> version) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(UpdateContentRecord::create(getCommandID(command), ref, getContentVersionID(version)));
    _steps++;
  }

  /// Handle an AddEntry IR step
  virtual void addEntry(const std::shared_ptr<Command>& command,
                        Ref::ID dir,
                        fs::path name,
                        Ref::ID target) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(AddEntryRecord::create(getCommandID(command), dir, name, target));
    _steps++;
  }

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const std::shared_ptr<Command>& command,
                           Ref::ID dir,
                           fs::path name,
                           Ref::ID target) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(RemoveEntryRecord::create(getCommandID(command), dir, name, target));
    _steps++;
  }

  /// Handle a Launch IR step
  virtual void launch(const std::shared_ptr<Command>& command,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(LaunchRecord::create(getCommandID(command), getCommandID(child), refs));
    _steps++;
  }

  /// Handle a Join IR step
  virtual void join(const std::shared_ptr<Command>& command,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(JoinRecord::create(getCommandID(command), getCommandID(child), exit_status));
    _steps++;
  }

  /// Handle an Exit IR step
  virtual void exit(const std::shared_ptr<Command>& command, int exit_status) noexcept override {
    ASSERT(_mode == Mode::Filling) << "Cannot add steps to a buffer that is draining";
    _archive(ExitRecord::create(getCommandID(command), exit_status));
    _steps++;
  }

  /**** IRLoader Methods ****/

  /// Identify a command with a given ID
  virtual void addCommand(Command::ID id, std::shared_ptr<Command> c) noexcept override;

  /// Identify a metadata version with a given ID
  virtual void addMetadataVersion(MetadataVersion::ID id,
                                  std::shared_ptr<MetadataVersion> mv) noexcept override;

  /// Identify a content version with a given ID
  virtual void addContentVersion(ContentVersion::ID id,
                                 std::shared_ptr<ContentVersion> cv) noexcept override;

  /// Get the ID for a command instance
  Command::ID getCommandID(const std::shared_ptr<Command>& c) noexcept;

  /// Get the ID for a metadata version
  MetadataVersion::ID getMetadataVersionID(const std::shared_ptr<MetadataVersion>& mv) noexcept;

  /// Get the ID for a content version
  ContentVersion::ID getContentVersionID(const std::shared_ptr<ContentVersion>& cv) noexcept;

 private:
  /// The temporary file descriptor used to hold this buffer's data
  int _fd;

  /// The output file stream
  std::ofstream _out;

  /// The cereal archive used to serialize steps
  cereal::BinaryOutputArchive _archive;

  /// What is this buffer's current mode? Is it filling, draining, or drained?
  Mode _mode = Mode::Filling;

  /// Keep track of the root command
  std::shared_ptr<Command> _root_command;

  /// Keep count of the steps in the buffer
  size_t _steps = 0;

  /// The map from commands to their IDs in the serialized data
  std::unordered_map<std::shared_ptr<Command>, Command::ID> _command_ids;

  /// The map from metadata versions to their IDs in the serialized data
  std::unordered_map<std::shared_ptr<MetadataVersion>, MetadataVersion::ID> _metadata_version_ids;

  /// The map from content versions to their IDs in the serialized data
  std::unordered_map<std::shared_ptr<ContentVersion>, ContentVersion::ID> _content_version_ids;
};