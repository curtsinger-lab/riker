#pragma once

#include <map>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>

#include "data/IRSink.hh"
#include "data/IRSource.hh"
#include "runtime/Command.hh"
#include "versions/ContentVersion.hh"

class MetadataVersion;

enum class RecordType : uint8_t;

class TraceReader : public IRSource {
 public:
 private:
};

class TraceWriter : public IRSink {
 public:
  /// Create a new TraceWriter with an optional output path. If no path is provided the trace is
  /// stored only in a temporary file.
  TraceWriter(std::optional<std::string> path = std::nullopt);

  /// Destroy a TraceWriter and clean up any remaining state
  virtual ~TraceWriter() noexcept override;

  // Disallow copy
  TraceWriter(const TraceWriter&) = delete;
  TraceWriter& operator=(const TraceWriter&) = delete;

  /// Called when starting a trace. The root command is passed in.
  virtual void start(const std::shared_ptr<Command>& c) noexcept override;

  /// Called when the trace is finished
  virtual void finish() noexcept override;

  /// Handle a SpecialRef IR step
  virtual void specialRef(const std::shared_ptr<Command>& command,
                          SpecialRef entity,
                          Ref::ID output) noexcept override;

  /// Handle a PipeRef IR step
  virtual void pipeRef(const std::shared_ptr<Command>& command,
                       Ref::ID read_end,
                       Ref::ID write_end) noexcept override;

  /// Handle a FileRef IR step
  virtual void fileRef(const std::shared_ptr<Command>& command,
                       mode_t mode,
                       Ref::ID output) noexcept override;

  /// Handle a SymlinkRef IR step
  virtual void symlinkRef(const std::shared_ptr<Command>& command,
                          fs::path target,
                          Ref::ID output) noexcept override;

  /// Handle a DirRef IR step
  virtual void dirRef(const std::shared_ptr<Command>& command,
                      mode_t mode,
                      Ref::ID output) noexcept override;

  /// Handle a PathRef IR step
  virtual void pathRef(const std::shared_ptr<Command>& command,
                       Ref::ID base,
                       fs::path path,
                       AccessFlags flags,
                       Ref::ID output) noexcept override;

  /// Handle a UsingRef IR step
  virtual void usingRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override;

  /// Handle a DoneWithRef IR step
  virtual void doneWithRef(const std::shared_ptr<Command>& command, Ref::ID ref) noexcept override;

  /// Handle a CompareRefs IR step
  virtual void compareRefs(const std::shared_ptr<Command>& command,
                           Ref::ID ref1,
                           Ref::ID ref2,
                           RefComparison type) noexcept override;

  /// Handle an ExpectResult IR step
  virtual void expectResult(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            int8_t expected) noexcept override;

  /// Handle a MatchMetadata IR step
  virtual void matchMetadata(const std::shared_ptr<Command>& command,
                             Scenario scenario,
                             Ref::ID ref,
                             MetadataVersion version) noexcept override;

  /// Handel a MatchContent IR step
  virtual void matchContent(const std::shared_ptr<Command>& command,
                            Scenario scenario,
                            Ref::ID ref,
                            std::shared_ptr<ContentVersion> version) noexcept override;

  /// Handle an UpdateMetadata IR step
  virtual void updateMetadata(const std::shared_ptr<Command>& command,
                              Ref::ID ref,
                              MetadataVersion version) noexcept override;

  /// Handle an UpdateContent IR step
  virtual void updateContent(const std::shared_ptr<Command>& command,
                             Ref::ID ref,
                             std::shared_ptr<ContentVersion> version) noexcept override;

  /// Handle an AddEntry IR step
  virtual void addEntry(const std::shared_ptr<Command>& command,
                        Ref::ID dir,
                        std::string name,
                        Ref::ID target) noexcept override;

  /// Handle a RemoveEntry IR step
  virtual void removeEntry(const std::shared_ptr<Command>& command,
                           Ref::ID dir,
                           std::string name,
                           Ref::ID target) noexcept override;

  /// Handle a Launch IR step
  virtual void launch(const std::shared_ptr<Command>& command,
                      const std::shared_ptr<Command>& child,
                      std::list<std::tuple<Ref::ID, Ref::ID>> refs) noexcept override;

  /// Handle a Join IR step
  virtual void join(const std::shared_ptr<Command>& command,
                    const std::shared_ptr<Command>& child,
                    int exit_status) noexcept override;

  /// Handle an Exit IR step
  virtual void exit(const std::shared_ptr<Command>& command, int exit_status) noexcept override;

 private:
  using StringID = uint16_t;
  using PathID = StringID;

  /// Write a sequence of values to the trace
  template <typename... T>
  void emit(T... args) noexcept;

  /// Emit a sequence of bytes to the trace
  void emitBytes(void* src, size_t len) noexcept;

  /// Get the ID of a command, possibly writing it to the output if it is new
  Command::ID getCommandID(const std::shared_ptr<Command>& command) noexcept;

  /// Emit a command to the trace
  void emitCommand(const std::shared_ptr<Command>& command) noexcept;

  /// Get the ID of a content version, possibly writing it to the output if it is new
  ContentVersion::ID getContentVersionID(const std::shared_ptr<ContentVersion>& version) noexcept;

  /// Emit a content version to the trace
  void emitContentVersion(const std::shared_ptr<ContentVersion>& v) noexcept;

  /// Make sure there is space for at least n strings in the current string table. If there isn't
  /// room in the current string table this will start a new one.
  void reserveStrings(size_t n) noexcept;

  /// Get the ID of a string, possibly writing it to the output if it is new
  StringID getStringID(const std::string& str) noexcept;

  /// Make sure there is space for at least n paths in the current path table. If there isn't
  /// room in the current path table this will start a new one.
  void reservePaths(size_t n) noexcept;

  /// Get the ID of a path, possibly writing it to the output if it is new
  PathID getPathID(const fs::path& path) noexcept;

 private:
  /// A unique identifier for this output trace
  size_t _id;

  /// The filename where this trace should be saved, or nullopt if the trace is not saved
  std::optional<std::string> _path = std::nullopt;

  int _fd = -1;              //< File descriptor for the backing file used to hold this trace
  size_t _length = 0;        //< The total size of the output trace
  size_t _pos = 0;           //< The current position in the output trace
  uint8_t* _data = nullptr;  //< A pointer to the beginning of the output trace mapping

  /// The map from commands to their IDs in the output trace
  std::map<std::shared_ptr<Command>, Command::ID> _commands;

  /// The map from content versions to their IDs in the output trace
  std::map<std::shared_ptr<ContentVersion>, ContentVersion::ID> _versions;

  /// The map from strings to their ID in the string table
  std::unordered_map<std::string, StringID> _strtab;
};
