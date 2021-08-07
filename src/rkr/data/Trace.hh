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
class FileVersion;
class SymlinkVersion;
class DirListVersion;
class PipeWriteVersion;
class PipeCloseVersion;
class PipeReadVersion;
class SpecialVersion;

enum class RecordType : uint8_t;

template <RecordType T>
struct NewRecord;

using StringID = uint16_t;
using PathID = StringID;

class TraceReader {
 public:
  /// Create a new TraceReader to load from a provided path
  TraceReader(std::string path) noexcept;

  /// Send a loaded trace to an IRSink
  void sendTo(IRSink& sink) noexcept;

  /// Accept r-value reference to a sink
  void sendTo(IRSink&& handler) noexcept { return sendTo(handler); }

 private:
  /// Check if we've hit the end of the trace
  bool done() const noexcept { return _pos >= _length; }

  /// Peek at the type of the next record
  RecordType peek() const noexcept;

  /// Get a reference to a record in the trace
  template <RecordType T>
  const NewRecord<T>& takeRecord() noexcept;

  /// Get a reference to data in the trace of a requested type
  template <typename T>
  const T& takeValue() noexcept;

  /// Get a reference to an array in the trace
  template <typename T>
  const T* takeArray(size_t count) noexcept;

  /// Get a pointer to a string in the trace and advance the current position past the string
  const char* takeString() noexcept;

  /// Handle a record from the trace (specialized in Trace.cc)
  template <RecordType T>
  void handleRecord(IRSink& sink) noexcept;

  /// Get a command from the table of commands
  const std::shared_ptr<Command>& getCommand(Command::ID id) const noexcept;

  /// Get a content version from the table of content versions
  const std::shared_ptr<ContentVersion>& getContentVersion(ContentVersion::ID id) const noexcept;

  /// Get a string from the table of strings
  const std::string& getString(StringID id) const noexcept;

 private:
  int _fd = -1;              //< File descriptor for the backing file used to hold this trace
  size_t _length = 0;        //< The total size of the output trace
  size_t _pos = 0;           //< The current position in the output trace
  uint8_t* _data = nullptr;  //< A pointer to the beginning of the output trace mapping

  /// The table of commands indexed by ID
  std::vector<std::shared_ptr<Command>> _commands;

  /// The table of content versions indexed by ID
  std::vector<std::shared_ptr<ContentVersion>> _versions;

  /// The table of strings indexed by ID
  std::vector<std::string> _strings;
};

class TraceWriter : public IRSink {
 public:
  /// Create a new TraceWriter with an optional output path. If no path is provided the trace is
  /// stored only in a temporary file.
  TraceWriter(std::optional<std::string> path = std::nullopt) noexcept;

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
  /// Write a record to the trace
  template <RecordType T, typename... Args>
  void emitRecord(Args... args) noexcept;

  /// Write a value to the trace
  template <typename T, typename... Args>
  void emitValue(Args... args) noexcept;

  /// Emit an array to the trace
  template <typename T>
  void emitArray(T* src, size_t count) noexcept;

  /// Get the ID of a command, possibly writing it to the output if it is new
  Command::ID getCommandID(const std::shared_ptr<Command>& command) noexcept;

  /// Emit a command to the trace
  void emitCommand(const std::shared_ptr<Command>& command) noexcept;

  /// Get the ID of a content version, possibly writing it to the output if it is new
  ContentVersion::ID getContentVersionID(const std::shared_ptr<ContentVersion>& version) noexcept;

  /// Emit a file version to the trace
  void emitFileVersion(const std::shared_ptr<FileVersion>& v) noexcept;

  /// Emit a symlink version to the trace
  void emitSymlinkVersion(const std::shared_ptr<SymlinkVersion>& v) noexcept;

  /// Emit a directory list version to the trace
  void emitDirListVersion(const std::shared_ptr<DirListVersion>& v) noexcept;

  /// Emit a pipe write version to the trace
  void emitPipeWriteVersion(const std::shared_ptr<PipeWriteVersion>& v) noexcept;

  /// Emit a pipe close version to the trace
  void emitPipeCloseVersion(const std::shared_ptr<PipeCloseVersion>& v) noexcept;

  /// Emit a pipe read version to the trace
  void emitPipeReadVersion(const std::shared_ptr<PipeReadVersion>& v) noexcept;

  /// Emit a special version to the trace
  void emitSpecialVersion(const std::shared_ptr<SpecialVersion>& v) noexcept;

  /// Emit a string record to the trace
  void emitString(const std::string& str) noexcept;

  /// Emit a new string table record to teh trace
  void emitNewStrtab() noexcept;

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
