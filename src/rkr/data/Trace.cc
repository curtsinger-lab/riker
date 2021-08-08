#include "Trace.hh"

#include <filesystem>
#include <limits>
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

#include <sys/mman.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "data/IRBuffer.hh"
#include "data/IRSink.hh"
#include "runtime/Command.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirListVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/PipeVersion.hh"
#include "versions/SpecialVersion.hh"
#include "versions/SymlinkVersion.hh"

namespace fs = std::filesystem;

using std::list;
using std::make_shared;
using std::nullopt;
using std::optional;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

/// Tags to identify each type of record
enum class RecordType : uint8_t {
  Start = 0,
  Finish = 1,
  SpecialRef = 2,
  PipeRef = 3,
  FileRef = 4,
  SymlinkRef = 5,
  DirRef = 6,
  PathRef = 7,
  UsingRef = 8,
  DoneWithRef = 9,
  CompareRefs = 10,
  ExpectResult = 11,
  MatchMetadata = 12,
  MatchContent = 13,
  UpdateMetadata = 14,
  UpdateContent = 15,
  AddEntry = 16,
  RemoveEntry = 17,
  Launch = 18,
  Join = 19,
  Exit = 20,
  Command = 21,
  String = 22,
  NewStrtab = 23,

  // Content version subtypes
  FileVersion = 32,
  SymlinkVersion = 33,
  DirListVersion = 34,
  PipeWriteVersion = 35,
  PipeCloseVersion = 36,
  PipeReadVersion = 37,
  SpecialVersion = 38
};

/********** TraceReader Constructor and Destructor **********/

optional<TraceReader> TraceReader::load(string path) noexcept {
  // Open the trace
  int fd = open(path.c_str(), O_RDONLY);
  if (fd == -1) return nullopt;

  // Get the length of the trace
  struct stat statbuf;
  int rc = fstat(fd, &statbuf);
  if (rc != 0) return nullopt;
  size_t length = statbuf.st_size;

  // Map the trace
  uint8_t* data = (uint8_t*)mmap(nullptr, length, PROT_READ, MAP_SHARED, fd, 0);
  if (data == MAP_FAILED) return nullopt;

  return TraceReader(fd, length, data);
}

// Create an empty trace reader
TraceReader::TraceReader() noexcept {
  setCommand(0, make_shared<Command>());
}

// Create a trace reader from an already open trace
TraceReader::TraceReader(int fd, size_t length, uint8_t* data) noexcept :
    _fd(fd), _length(length), _pos(0), _data(data) {
  setCommand(0, make_shared<Command>());
}

TraceReader::~TraceReader() noexcept {
  // Is there an open file? If not just return.
  if (_fd == -1) return;

  // Close the trace file
  int rc = close(_fd);
  WARN_IF(rc != 0) << "Failed to close trace: " << ERR;

  // Unmap the trace file to make sure it's written out to disk
  rc = munmap(_data, _length);
  WARN_IF(rc != 0) << "Failed to munmap trace: " << ERR;
}

// Allow move
TraceReader::TraceReader(TraceReader&& other) noexcept :
    _fd(other._fd),
    _length(other._length),
    _pos(other._pos),
    _data(other._data),
    _commands(std::move(other._commands)),
    _versions(std::move(other._versions)),
    _strings(std::move(other._strings)) {
  other._fd = -1;
  other._length = 0;
  other._pos = 0;
  other._data = nullptr;
}

TraceReader& TraceReader::operator=(TraceReader&& other) noexcept {
  // Does this TraceReader already have an open file?
  if (_fd != -1) {
    // Close the trace file
    int rc = close(_fd);
    FAIL_IF(rc != 0) << "Failed to close trace: " << ERR;

    // Unmap the trace file to make sure it's written out to disk
    rc = munmap(_data, _length);
    WARN_IF(rc != 0) << "Failed to munmap trace: " << ERR;
  }

  // Adopt the other trace's state
  _fd = other._fd;
  other._fd = -1;

  _length = other._length;
  other._length = 0;

  _pos = other._pos;
  other._pos = 0;

  _data = other._data;
  other._data = nullptr;

  _commands = std::move(other._commands);
  _versions = std::move(other._versions);
  _strings = std::move(other._strings);

  return *this;
}

shared_ptr<Command> TraceReader::getRootCommand() const noexcept {
  return _commands[0];
}

/********** TraceWriter Constructor and Destructor **********/

TraceWriter::TraceWriter(optional<string> path) noexcept : _id(IRBuffer::getNextID()), _path(path) {
  // Create a temporary file to hold the trace
  // TODO: if a path was provided, get the containing directory name to make sure the trace is
  // stored on the same device so it can be linked later
  _fd = open(".", O_RDWR | O_TMPFILE, 0644);
  FAIL_IF(_fd == -1) << "Failed to open temporary file: " << ERR;

  // Extend the trace to 256MB. Eventually it needs to grow automatically
  _length = 256 * 1024 * 1024;
  int rc = ftruncate(_fd, _length);
  FAIL_IF(rc != 0) << "Failed to extend file to 256MB: " << ERR;

  // Map the file
  _data = (uint8_t*)mmap(nullptr, _length, PROT_READ | PROT_WRITE, MAP_SHARED, _fd, 0);
  FAIL_IF(_data == MAP_FAILED) << "Failed to mmap file: " << ERR;
}

TraceWriter::~TraceWriter() noexcept {
  // Is there an open file? If not, just return
  if (_fd == -1) return;

  // Was a path provided?
  if (_path.has_value()) {
    // Yes. Link the trace onto the filesystem before it vanishes

    // First make sure the output path doesn't exist
    int rc = ::unlink(_path.value().c_str());

    // The output file may not exist, but if the unlink failed for some other reason give up
    FAIL_IF(rc != 0 && errno != ENOENT)
        << "Failed to unlink old trace output file " << _path.value() << ": " << ERR;

    // Now link in the temporary file from the /proc filesystem
    string fdpath = "/proc/self/fd/" + std::to_string(_fd);
    rc = linkat(AT_FDCWD, fdpath.c_str(), AT_FDCWD, _path.value().c_str(), AT_SYMLINK_FOLLOW);

    // TODO: if linking fails, fall back on copying
    FAIL_IF(rc != 0) << "Failed to link trace from " << fdpath << " to " << _path.value();
  }

  // Discard any unused capacity in the trace
  int rc = ftruncate(_fd, _pos);
  WARN_IF(rc != 0) << "Failed to truncate trace: " << ERR;

  // Close the trace file
  rc = close(_fd);
  WARN_IF(rc != 0) << "Failed to close trace: " << ERR;

  // Unmap the trace file to make sure it's written out to disk
  rc = munmap(_data, _length);
  WARN_IF(rc != 0) << "Failed to munmap trace: " << ERR;
}

// Create a TraceReader to traverse this trace. Makes the writer unusable
TraceReader TraceWriter::getReader() noexcept {
  // Create a trace reader
  TraceReader result(_fd, _pos, _data);

  // Transfer commands over to the reader
  for (const auto& [c, id] : _commands) {
    result.setCommand(id, c);
  }

  // Transfer content versions over to the reader
  for (const auto& [v, id] : _versions) {
    result.setVersion(id, v);
  }

  // Clear the local fields
  _fd = -1;
  _pos = 0;
  _data = nullptr;
  _length = 0;

  // Return the new reader
  return result;
}

/********** TraceReader Reading Methods **********/

// Look at the type of the next record without advancing the current position
RecordType TraceReader::peek() const noexcept {
  RecordType* p = reinterpret_cast<RecordType*>(&_data[_pos]);
  return *p;
}

// Get a reference to a record in the input trace
template <RecordType T>
const NewRecord<T>& TraceReader::takeRecord() noexcept {
  return takeValue<NewRecord<T>>();
}

// Get reference to data in the trace of a requested type
template <typename T>
const T& TraceReader::takeValue() noexcept {
  T* p = reinterpret_cast<T*>(&_data[_pos]);
  _pos += sizeof(T);
  return *p;
}

// Get a reference to an array in the trace
template <typename T>
const T* TraceReader::takeArray(size_t count) noexcept {
  T* p = reinterpret_cast<T*>(&_data[_pos]);
  _pos += count * sizeof(T);
  return p;
}

// Get a pointer to a string and advance the current position to the end of the string
const char* TraceReader::takeString() noexcept {
  const char* current = reinterpret_cast<const char*>(&_data[_pos]);
  const char* result = current;
  while (*current != '\0') {
    current++;
    _pos++;
  }
  _pos++;
  return result;
}

/********** TraceWriter Writing Methods **********/

// Write a record to the trace
template <RecordType T, typename... Args>
void TraceWriter::emitRecord(Args... args) noexcept {
  using R = NewRecord<T>;
  R* r = reinterpret_cast<R*>(&_data[_pos]);
  *r = R{T, args...};
  _pos += sizeof(R);
}

// Write a value to the trace
template <typename T, typename... Args>
void TraceWriter::emitValue(Args... args) noexcept {
  T* p = reinterpret_cast<T*>(&_data[_pos]);
  *p = T{args...};
  _pos += sizeof(T);
}

// Emit an array to the trace
template <typename T>
void TraceWriter::emitArray(T* src, size_t count) noexcept {
  void* dest = &_data[_pos];
  memcpy(dest, src, sizeof(T) * count);
  _pos += sizeof(T) * count;
}

/********** Instance ID Methods **********/

// Get a command from the table of commands
const shared_ptr<Command>& TraceReader::getCommand(Command::ID id) const noexcept {
  return _commands[id];
}

Command::ID TraceWriter::getCommandID(const std::shared_ptr<Command>& c) noexcept {
  auto id = c->getID(_id);
  if (id.has_value()) return id.value();

  // Look for the provided command in the map of known commands
  auto iter = _commands.find(c);
  if (iter == _commands.end()) {
    // The command was not found. Add it now
    Command::ID id = _commands.size();
    iter = _commands.emplace_hint(iter, c, id);

    // Write the command to the trace
    emitCommand(c);
  }

  c->setID(_id, iter->second);
  return iter->second;
}

// Set a command in the commands table using a known ID
void TraceReader::setCommand(Command::ID id, std::shared_ptr<Command> c) noexcept {
  if (_commands.size() <= id) _commands.resize(id + 1);
  _commands[id] = c;
}

// Add a command to the commands table and assign a new ID
void TraceReader::addCommand(std::shared_ptr<Command> c) noexcept {
  // Assign an ID for the new command
  size_t id = _next_command_id++;

  // Make sure the commands array has space for the new command
  if (id >= _commands.size()) _commands.resize(id + 1);

  // If the command isn't already stored, store it
  if (!_commands[id]) _commands[id] = c;
}

// Get a content version from the table of content versions
const shared_ptr<ContentVersion>& TraceReader::getContentVersion(
    ContentVersion::ID id) const noexcept {
  return _versions[id];
}

// Get the ID for a content version. Emit a new metadata version record if necessary
ContentVersion::ID TraceWriter::getContentVersionID(const shared_ptr<ContentVersion>& v) noexcept {
  auto id = v->getID(_id);
  if (id.has_value()) return id.value();

  // Look for the provided content version in the map of known versions
  auto iter = _versions.find(v);
  if (iter == _versions.end()) {
    // If the version wasn't found, add it now
    ContentVersion::ID id = _versions.size();
    iter = _versions.emplace_hint(iter, v, id);

    // Write the content version to the trace
    if (auto fv = v->as<FileVersion>(); fv) {
      emitFileVersion(fv);

    } else if (auto sv = v->as<SymlinkVersion>(); sv) {
      emitSymlinkVersion(sv);

    } else if (auto dv = v->as<DirListVersion>(); dv) {
      emitDirListVersion(dv);

    } else if (auto pv = v->as<PipeWriteVersion>(); pv) {
      emitPipeWriteVersion(pv);

    } else if (auto pv = v->as<PipeCloseVersion>(); pv) {
      emitPipeCloseVersion(pv);

    } else if (auto pv = v->as<PipeReadVersion>(); pv) {
      emitPipeReadVersion(pv);

    } else if (auto sv = v->as<SpecialVersion>(); sv) {
      emitSpecialVersion(sv);

    } else {
      FAIL << "Unrecognized version type " << v;
    }
  }

  // Return the ID
  v->setID(_id, iter->second);
  return iter->second;
}

// Set a content version in the versions table using a known ID
void TraceReader::setVersion(ContentVersion::ID id, std::shared_ptr<ContentVersion> v) noexcept {
  if (_versions.size() <= id) _versions.resize(id + 1);
  _versions[id] = v;
}

// Add a content version to the table and assign a new ID
void TraceReader::addVersion(std::shared_ptr<ContentVersion> v) noexcept {
  // Assign an ID for the new version
  size_t id = _next_version_id++;

  // Make sure the versions array has space for the new version
  if (id >= _versions.size()) _versions.resize(id + 1);

  // If the version isn't already stored, store it
  if (!_versions[id]) _versions[id] = v;
}

/********** String and Path Table Methods **********/

void TraceWriter::reserveStrings(size_t n) noexcept {
  ASSERT(n < std::numeric_limits<StringID>::max())
      << "Requested number of strings is larger than the total string table size";

  // Check if the string table will fill before n strings are emitted
  if (std::numeric_limits<StringID>::max() - _strtab.size() < n) {
    // Make room by starting a fresh string table
    emitNewStrtab();
  }
}

void TraceWriter::reservePaths(size_t n) noexcept {
  reserveStrings(n);
}

/// Get a string from the table of strings
const string& TraceReader::getString(StringID id) const noexcept {
  return _strings[id];
}

StringID TraceWriter::getStringID(const std::string& str) noexcept {
  // Look for this string in the string table
  auto iter = _strtab.find(str);
  if (iter != _strtab.end()) {
    // Found it. Return the ID
    return iter->second;

  } else {
    // The string was not found. Assign an ID
    StringID id = _strtab.size();

    // Is the string table full?
    if (id >= std::numeric_limits<StringID>::max()) {
      // Start a fresh string table
      emitNewStrtab();
      id = 0;
    }

    _strtab.emplace_hint(iter, str, id);

    // Write out the string record
    emitString(str);

    return id;
  }
}

PathID TraceWriter::getPathID(const fs::path& path) noexcept {
  return getStringID(path.string());
}

/********** Start Record **********/

template <>
struct NewRecord<RecordType::Start> {
  RecordType type;
  Command::ID root_command;
} __attribute__((packed));

// Read a Start record from the input trace
template <>
void TraceReader::handleRecord<RecordType::Start>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::Start>();
  sink.start(getCommand(data.root_command));
}

// Write a Start record to the output trace
void TraceWriter::start(const shared_ptr<Command>& c) noexcept {
  emitRecord<RecordType::Start>(getCommandID(c));
}

/********** Finish Record **********/

template <>
struct NewRecord<RecordType::Finish> {
  RecordType type;
} __attribute__((packed));

// Read a Finish record from the input trace
template <>
void TraceReader::handleRecord<RecordType::Finish>(IRSink& sink) noexcept {
  takeRecord<RecordType::Finish>();
  sink.finish();
}

// Write a Finish record to the output trace
void TraceWriter::finish() noexcept {
  emitRecord<RecordType::Finish>();
}

/********** SpecialRef Record **********/

template <>
struct NewRecord<RecordType::SpecialRef> {
  RecordType type;
  Command::ID command;
  SpecialRef entity;
  Ref::ID output;
} __attribute__((packed));

// Read a SpecialRef record from the input trace
template <>
void TraceReader::handleRecord<RecordType::SpecialRef>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::SpecialRef>();
  sink.specialRef(getCommand(data.command), data.entity, data.output);
}

// Write a SpecialRef record to the output trace
void TraceWriter::specialRef(const shared_ptr<Command>& c,
                             SpecialRef entity,
                             Ref::ID output) noexcept {
  emitRecord<RecordType::SpecialRef>(getCommandID(c), entity, output);
}

/********** PipeRef Record **********/

template <>
struct NewRecord<RecordType::PipeRef> {
  RecordType type;
  Command::ID command;
  Ref::ID read_end;
  Ref::ID write_end;
} __attribute__((packed));

// Read a PipeRef record from the input trace
template <>
void TraceReader::handleRecord<RecordType::PipeRef>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::PipeRef>();
  sink.pipeRef(getCommand(data.command), data.read_end, data.write_end);
}

// Write a PipeRef record to the output trace
void TraceWriter::pipeRef(const shared_ptr<Command>& c,
                          Ref::ID read_end,
                          Ref::ID write_end) noexcept {
  emitRecord<RecordType::PipeRef>(getCommandID(c), read_end, write_end);
}

/********** FileRef Record **********/

template <>
struct NewRecord<RecordType::FileRef> {
  RecordType type;
  Command::ID command;
  mode_t mode;
  Ref::ID output;
} __attribute__((packed));

// Read a FileRef record from the input trace
template <>
void TraceReader::handleRecord<RecordType::FileRef>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::FileRef>();
  sink.fileRef(getCommand(data.command), data.mode, data.output);
}

// Write a FileRef record to the output trace
void TraceWriter::fileRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept {
  emitRecord<RecordType::FileRef>(getCommandID(c), mode, output);
}

/********** SymlinkRef Record **********/

template <>
struct NewRecord<RecordType::SymlinkRef> {
  RecordType type;
  Command::ID command;
  PathID target;
  Ref::ID output;
} __attribute__((packed));

// Read a SymlinkRef record from the input trace
template <>
void TraceReader::handleRecord<RecordType::SymlinkRef>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::SymlinkRef>();
  sink.symlinkRef(getCommand(data.command), getString(data.target), data.output);
}

// Write a SymlinkRef record to the output trace
void TraceWriter::symlinkRef(const shared_ptr<Command>& c,
                             fs::path target,
                             Ref::ID output) noexcept {
  emitRecord<RecordType::SymlinkRef>(getCommandID(c), getPathID(target), output);
}

/********** DirRef Record **********/

template <>
struct NewRecord<RecordType::DirRef> {
  RecordType type;
  Command::ID command;
  mode_t mode;
  Ref::ID output;
} __attribute__((packed));

// Read a DirRef record from the input trace
template <>
void TraceReader::handleRecord<RecordType::DirRef>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::DirRef>();
  sink.dirRef(getCommand(data.command), data.mode, data.output);
}

// Write a DirRef record to the output trace
void TraceWriter::dirRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept {
  emitRecord<RecordType::DirRef>(getCommandID(c), mode, output);
}

/********** PathRef Record **********/

template <>
struct NewRecord<RecordType::PathRef> {
  RecordType type;
  Command::ID command;
  Ref::ID base;
  PathID path;
  AccessFlags flags;
  Ref::ID output;
} __attribute__((packed));

// Read a PathRef record from the input trace
template <>
void TraceReader::handleRecord<RecordType::PathRef>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::PathRef>();
  sink.pathRef(getCommand(data.command), data.base, getString(data.path), data.flags, data.output);
}

// Write a PathRef record to the output trace
void TraceWriter::pathRef(const shared_ptr<Command>& c,
                          Ref::ID base,
                          fs::path path,
                          AccessFlags flags,
                          Ref::ID output) noexcept {
  emitRecord<RecordType::PathRef>(getCommandID(c), base, getPathID(path), flags, output);
}

/********** UsingRef Record **********/

template <>
struct NewRecord<RecordType::UsingRef> {
  RecordType type;
  Command::ID command;
  Ref::ID ref;
} __attribute__((packed));

// Read a UsingRef record from the input trace
template <>
void TraceReader::handleRecord<RecordType::UsingRef>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::UsingRef>();
  sink.usingRef(getCommand(data.command), data.ref);
}

// Write a UsingRef record to the output trace
void TraceWriter::usingRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  emitRecord<RecordType::UsingRef>(getCommandID(c), ref);
}

/********** DoneWithRef Record **********/

template <>
struct NewRecord<RecordType::DoneWithRef> {
  RecordType type;
  Command::ID command;
  Ref::ID ref;
} __attribute__((packed));

// Read a DoneWithRef record from the input trace
template <>
void TraceReader::handleRecord<RecordType::DoneWithRef>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::DoneWithRef>();
  sink.doneWithRef(getCommand(data.command), data.ref);
}

// Write a DoneWithRef record to the output trace
void TraceWriter::doneWithRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  emitRecord<RecordType::DoneWithRef>(getCommandID(c), ref);
}

/********** CompareRefs Record **********/

template <>
struct NewRecord<RecordType::CompareRefs> {
  RecordType type;
  Command::ID command;
  Ref::ID ref1;
  Ref::ID ref2;
  RefComparison cmp;
} __attribute__((packed));

// Read a CompareRefs record from the input trace
template <>
void TraceReader::handleRecord<RecordType::CompareRefs>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::CompareRefs>();
  sink.compareRefs(getCommand(data.command), data.ref1, data.ref2, data.cmp);
}

// Write a CompareRefs record to the output trace
void TraceWriter::compareRefs(const shared_ptr<Command>& c,
                              Ref::ID ref1,
                              Ref::ID ref2,
                              RefComparison type) noexcept {
  emitRecord<RecordType::CompareRefs>(getCommandID(c), ref1, ref2, type);
}

/********** ExpectResult Record **********/

template <>
struct NewRecord<RecordType::ExpectResult> {
  RecordType type;
  Command::ID command;
  Scenario scenario;
  Ref::ID ref;
  int8_t expected;
} __attribute__((packed));

// Read an ExpectResult record from the input trace
template <>
void TraceReader::handleRecord<RecordType::ExpectResult>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::ExpectResult>();
  sink.expectResult(getCommand(data.command), data.scenario, data.ref, data.expected);
}

// Write an ExpectResult record to the output trace
void TraceWriter::expectResult(const shared_ptr<Command>& c,
                               Scenario scenario,
                               Ref::ID ref,
                               int8_t expected) noexcept {
  emitRecord<RecordType::ExpectResult>(getCommandID(c), scenario, ref, expected);
}

/********** MatchMetadata Record **********/

template <>
struct NewRecord<RecordType::MatchMetadata> {
  RecordType type;
  Command::ID command;
  Scenario scenario;
  Ref::ID ref;
  MetadataVersion version;
} __attribute__((packed));

// Read a MatchMetadata record from the input trace
template <>
void TraceReader::handleRecord<RecordType::MatchMetadata>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::MatchMetadata>();
  sink.matchMetadata(getCommand(data.command), data.scenario, data.ref, data.version);
}

// Write a MatchMetadata record to the output trace
void TraceWriter::matchMetadata(const shared_ptr<Command>& c,
                                Scenario scenario,
                                Ref::ID ref,
                                MetadataVersion version) noexcept {
  emitRecord<RecordType::MatchMetadata>(getCommandID(c), scenario, ref, version);
}

/********** MatchContent Record **********/

template <>
struct NewRecord<RecordType::MatchContent> {
  RecordType type;
  Command::ID command;
  Scenario scenario;
  Ref::ID ref;
  ContentVersion::ID version;
} __attribute__((packed));

// Read a MatchContent record from the input trace
template <>
void TraceReader::handleRecord<RecordType::MatchContent>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::MatchContent>();
  sink.matchContent(getCommand(data.command), data.scenario, data.ref,
                    getContentVersion(data.version));
}

// Write a MatchContent record to the output trace
void TraceWriter::matchContent(const shared_ptr<Command>& c,
                               Scenario scenario,
                               Ref::ID ref,
                               shared_ptr<ContentVersion> version) noexcept {
  emitRecord<RecordType::MatchContent>(getCommandID(c), scenario, ref,
                                       getContentVersionID(version));
}

/********** UpdateMetadata Record **********/

template <>
struct NewRecord<RecordType::UpdateMetadata> {
  RecordType type;
  Command::ID command;
  Ref::ID ref;
  MetadataVersion version;
} __attribute__((packed));

// Read an UpdateMetadata record from the input trace
template <>
void TraceReader::handleRecord<RecordType::UpdateMetadata>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::UpdateMetadata>();
  sink.updateMetadata(getCommand(data.command), data.ref, data.version);
}

// Write an UpdateMetadata record to the output trace
void TraceWriter::updateMetadata(const shared_ptr<Command>& c,
                                 Ref::ID ref,
                                 MetadataVersion version) noexcept {
  emitRecord<RecordType::UpdateMetadata>(getCommandID(c), ref, version);
}

/********** UpdateContent Record **********/

template <>
struct NewRecord<RecordType::UpdateContent> {
  RecordType type;
  Command::ID command;
  Ref::ID ref;
  ContentVersion::ID version;
} __attribute__((packed));

// Read an UpdateContent record from the input trace
template <>
void TraceReader::handleRecord<RecordType::UpdateContent>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::UpdateContent>();
  sink.updateContent(getCommand(data.command), data.ref, getContentVersion(data.version));
}

// Write an UpdateContent record to the output trace
void TraceWriter::updateContent(const shared_ptr<Command>& c,
                                Ref::ID ref,
                                shared_ptr<ContentVersion> version) noexcept {
  emitRecord<RecordType::UpdateContent>(getCommandID(c), ref, getContentVersionID(version));
}

/********** AddEntry Record **********/

template <>
struct NewRecord<RecordType::AddEntry> {
  RecordType type;
  Command::ID command;
  Ref::ID dir;
  StringID name;
  Ref::ID target;
} __attribute__((packed));

// Read an AddEntry record from the input trace
template <>
void TraceReader::handleRecord<RecordType::AddEntry>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::AddEntry>();
  sink.addEntry(getCommand(data.command), data.dir, getString(data.name), data.target);
}

// Write an AddEntry record to the output trace
void TraceWriter::addEntry(const shared_ptr<Command>& c,
                           Ref::ID dir,
                           string name,
                           Ref::ID target) noexcept {
  emitRecord<RecordType::AddEntry>(getCommandID(c), dir, getStringID(name), target);
}

/********** RemoveEntry Record **********/

template <>
struct NewRecord<RecordType::RemoveEntry> {
  RecordType type;
  Command::ID command;
  Ref::ID dir;
  StringID name;
  Ref::ID target;
} __attribute__((packed));

// Read a RemoveEntry record from the input trace
template <>
void TraceReader::handleRecord<RecordType::RemoveEntry>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::RemoveEntry>();
  sink.removeEntry(getCommand(data.command), data.dir, getString(data.name), data.target);
}

// Write a RemoveEntry record to the output trace
void TraceWriter::removeEntry(const shared_ptr<Command>& c,
                              Ref::ID dir,
                              string name,
                              Ref::ID target) noexcept {
  emitRecord<RecordType::RemoveEntry>(getCommandID(c), dir, getStringID(name), target);
}

/********** Launch Record **********/

template <>
struct NewRecord<RecordType::Launch> {
  RecordType type;
  Command::ID parent;
  Command::ID child;
  uint16_t refs_length;
} __attribute__((packed));

struct RefMapping {
  Ref::ID in_parent;
  Ref::ID in_child;
} __attribute__((packed));

// Read a Launch record from the input trace
template <>
void TraceReader::handleRecord<RecordType::Launch>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::Launch>();
  const RefMapping* refs = takeArray<RefMapping>(data.refs_length);

  list<tuple<Ref::ID, Ref::ID>> refs_list;
  for (size_t i = 0; i < data.refs_length; i++) {
    refs_list.push_back(tuple{refs[i].in_parent, refs[i].in_child});
  }

  sink.launch(getCommand(data.parent), getCommand(data.child), refs_list);
}

// Write a Launch record to the output trace
void TraceWriter::launch(const shared_ptr<Command>& parent,
                         const shared_ptr<Command>& child,
                         list<tuple<Ref::ID, Ref::ID>> refs) noexcept {
  // Compute the length of the ref mapping list, which should fit in a 16-bit integer
  uint16_t refs_length = refs.size();

  // Emit the fixed-length portion of the record
  emitRecord<RecordType::Launch>(getCommandID(parent), getCommandID(child), refs_length);

  // Now emit the ref mappings
  for (auto [a, b] : refs) {
    emitValue<RefMapping>(a, b);
  }
}

/********** Join Record **********/

template <>
struct NewRecord<RecordType::Join> {
  RecordType type;
  Command::ID parent;
  Command::ID child;
  int exit_status;
} __attribute__((packed));

// Read a Join record from the input trace
template <>
void TraceReader::handleRecord<RecordType::Join>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::Join>();
  sink.join(getCommand(data.parent), getCommand(data.child), data.exit_status);
}

// Write a Join record to the output trace
void TraceWriter::join(const shared_ptr<Command>& parent,
                       const shared_ptr<Command>& child,
                       int exit_status) noexcept {
  emitRecord<RecordType::Join>(getCommandID(parent), getCommandID(child), exit_status);
}

/********** Exit Record **********/

template <>
struct NewRecord<RecordType::Exit> {
  RecordType type;
  Command::ID command;
  int exit_status;
} __attribute__((packed));

// Read an Exit record from the input trace
template <>
void TraceReader::handleRecord<RecordType::Exit>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::Exit>();
  sink.exit(getCommand(data.command), data.exit_status);
}

// Write an Exit record to the output trace
void TraceWriter::exit(const shared_ptr<Command>& c, int exit_status) noexcept {
  emitRecord<RecordType::Exit>(getCommandID(c), exit_status);
}

/********** Command Record **********/

// The fixed-size data written for each command in the trace
template <>
struct NewRecord<RecordType::Command> {
  RecordType type;
  bool has_executed;
  uint16_t argv_length;
  uint16_t initial_fds_length;
} __attribute__((packed));

// A struct used to map a file descriptor to a reference ID
struct FDRecord2 {
  int fd;
  Ref::ID ref;
} __attribute__((packed));

// Read a Command record from the input trace
template <>
void TraceReader::handleRecord<RecordType::Command>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::Command>();
  const StringID* arg_ids = takeArray<StringID>(data.argv_length);
  const FDRecord2* fds = takeArray<FDRecord2>(data.initial_fds_length);

  // Get argument strings
  vector<string> args;
  for (size_t i = 0; i < data.argv_length; i++) {
    args.push_back(getString(arg_ids[i]));
  }

  // Create a command
  auto cmd = make_shared<Command>(args);
  if (data.has_executed) cmd->setExecuted();

  // Add initial file descriptors
  for (size_t i = 0; i < data.initial_fds_length; i++) {
    cmd->addInitialFD(fds[i].fd, fds[i].ref);
  }

  // Save the command in the commands table
  addCommand(cmd);
}

// Write a Command record to the output trace
void TraceWriter::emitCommand(const std::shared_ptr<Command>& c) noexcept {
  // We're going to emit argv strings. Make sure there's room for all of them in the string table
  reserveStrings(c->getArguments().size());

  // Emit each of the strings in the argv array
  vector<StringID> args;
  for (const auto& arg : c->getArguments()) {
    args.push_back(getStringID(arg));
  }

  // Get the lengths of the variable-length parts of a command record
  uint16_t argv_length = args.size();
  uint16_t initial_fds_length = c->getInitialFDs().size();

  // Write out the fixed-length portion of the command record
  emitRecord<RecordType::Command>(c->hasExecuted(), argv_length, initial_fds_length);

  // Write out the argv string IDs
  emitArray(args.data(), args.size());

  // Write out the initial FDs
  for (auto [fd, ref] : c->getInitialFDs()) {
    emitValue<FDRecord2>(fd, ref);
  }
}

/********** String Record **********/

template <>
struct NewRecord<RecordType::String> {
  RecordType type;
} __attribute__((packed));

// Read a String record from the input trace
template <>
void TraceReader::handleRecord<RecordType::String>(IRSink& sink) noexcept {
  takeRecord<RecordType::String>();
  const char* str = takeString();
  _strings.emplace_back(str);
}

// Write a String record to the output trace
void TraceWriter::emitString(const string& str) noexcept {
  // Write out the string record
  emitRecord<RecordType::String>();
  emitArray(str.c_str(), str.size() + 1);
}

/********** NewStrtab Record **********/

template <>
struct NewRecord<RecordType::NewStrtab> {
  RecordType type;
} __attribute__((packed));

// Read a NewStrtab record from the input trace
template <>
void TraceReader::handleRecord<RecordType::NewStrtab>(IRSink& sink) noexcept {
  takeRecord<RecordType::NewStrtab>();
  _strings.clear();
}

// Write a NewStrtab record to the output trace
void TraceWriter::emitNewStrtab() noexcept {
  emitRecord<RecordType::NewStrtab>();
  _strtab.clear();
}

/********** FileVersion Record **********/

template <>
struct NewRecord<RecordType::FileVersion> {
  RecordType type;
  bool is_empty : 1;
  bool is_cached : 1;
  bool has_mtime : 1;
  bool has_hash : 1;
  struct timespec mtime;
  FileVersion::Hash hash;
} __attribute__((packed));

// Read a FileVersion record from the input trace
template <>
void TraceReader::handleRecord<RecordType::FileVersion>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::FileVersion>();

  optional<struct timespec> mtime;
  if (data.has_mtime) mtime = data.mtime;

  optional<FileVersion::Hash> hash;
  if (data.has_hash) hash = data.hash;

  addVersion(make_shared<FileVersion>(data.is_empty, data.is_cached, mtime, hash));
}

// Write a FileVersion record to the output trace
void TraceWriter::emitFileVersion(const shared_ptr<FileVersion>& v) noexcept {
  // Does the version have an mtime and/or hash?
  bool has_mtime = v->getModificationTime().has_value();
  auto mtime = v->getModificationTime().value_or(timespec{0, 0});
  bool has_hash = v->getHash().has_value();
  auto hash = v->getHash().value_or(FileVersion::Hash());

  // Emit the file version
  emitRecord<RecordType::FileVersion>(v->isEmpty(), v->isCached(), has_mtime, has_hash, mtime,
                                      hash);
}

/********** SymlinkVersion Record **********/

template <>
struct NewRecord<RecordType::SymlinkVersion> {
  RecordType type;
  StringID dest;
} __attribute__((packed));

// Read a SymlinkVersion record from the input trace
template <>
void TraceReader::handleRecord<RecordType::SymlinkVersion>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::SymlinkVersion>();
  addVersion(make_shared<SymlinkVersion>(getString(data.dest)));
}

// Write a SymlinkVersion record to the output trace
void TraceWriter::emitSymlinkVersion(const shared_ptr<SymlinkVersion>& v) noexcept {
  emitRecord<RecordType::SymlinkVersion>(getPathID(v->getDestination()));
}

/********** DirListVersion Record **********/

template <>
struct NewRecord<RecordType::DirListVersion> {
  RecordType type;
  uint16_t entry_count;
} __attribute__((packed));

// Read a DirListVersion record from the input trace
template <>
void TraceReader::handleRecord<RecordType::DirListVersion>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::DirListVersion>();
  const PathID* entry_ids = takeArray<PathID>(data.entry_count);

  auto v = make_shared<DirListVersion>();
  for (size_t i = 0; i < data.entry_count; i++) {
    v->addEntry(getString(entry_ids[i]));
  }

  addVersion(v);
}

// Write a DirListVersion record to the output trace
void TraceWriter::emitDirListVersion(const shared_ptr<DirListVersion>& v) noexcept {
  // Get the number of directory entries, which should fit in a uint16_t
  uint16_t entry_count = v->getEntries().size();

  ASSERT(entry_count == v->getEntries().size())
      << "A directory has too many entries to fit in a uint16_t...";

  // Now build a vector of IDs for each of the paths
  vector<PathID> entries;
  entries.reserve(entry_count);

  // Reserve enough paths so they all fit in the current path/string table
  reservePaths(entry_count);
  for (const auto& entry : v->getEntries()) {
    entries.push_back(getPathID(entry));
  }

  // Write out the fixed-length portion of the directory list version
  emitRecord<RecordType::DirListVersion>(entry_count);

  // And write out the entry string ID list
  emitArray(entries.data(), entries.size());
}

/********** PipeWriteVersion Record **********/

template <>
struct NewRecord<RecordType::PipeWriteVersion> {
  RecordType type;
} __attribute__((packed));

// Read a PipeWriteVersion record from the input trace
template <>
void TraceReader::handleRecord<RecordType::PipeWriteVersion>(IRSink& sink) noexcept {
  takeRecord<RecordType::PipeWriteVersion>();
  addVersion(make_shared<PipeWriteVersion>());
}

// Write a PipeWriteVersion record to the output trace
void TraceWriter::emitPipeWriteVersion(const shared_ptr<PipeWriteVersion>& v) noexcept {
  emitRecord<RecordType::PipeWriteVersion>();
}

/********** PipeCloseVersion Record **********/

template <>
struct NewRecord<RecordType::PipeCloseVersion> {
  RecordType type;
} __attribute__((packed));

// Read a PipeCloseVersion record from the input trace
template <>
void TraceReader::handleRecord<RecordType::PipeCloseVersion>(IRSink& sink) noexcept {
  takeRecord<RecordType::PipeCloseVersion>();
  addVersion(make_shared<PipeCloseVersion>());
}

// Write a PipeCloseVersion record to the output trace
void TraceWriter::emitPipeCloseVersion(const shared_ptr<PipeCloseVersion>& v) noexcept {
  emitRecord<RecordType::PipeCloseVersion>();
}

/********** PipeReadVersion Record **********/

template <>
struct NewRecord<RecordType::PipeReadVersion> {
  RecordType type;
} __attribute__((packed));

// Read a PipeReadVersion record from the input trace
template <>
void TraceReader::handleRecord<RecordType::PipeReadVersion>(IRSink& sink) noexcept {
  takeRecord<RecordType::PipeReadVersion>();
  addVersion(make_shared<PipeReadVersion>());
}

// Write a PipeReadVersion record to the output trace
void TraceWriter::emitPipeReadVersion(const shared_ptr<PipeReadVersion>& v) noexcept {
  emitRecord<RecordType::PipeReadVersion>();
}

/********** SpecialVersion Record **********/

template <>
struct NewRecord<RecordType::SpecialVersion> {
  RecordType type;
  bool can_commit;
} __attribute__((packed));

// Read a SpecialVersion record from the input trace
template <>
void TraceReader::handleRecord<RecordType::SpecialVersion>(IRSink& sink) noexcept {
  const auto& data = takeRecord<RecordType::SpecialVersion>();
  addVersion(make_shared<SpecialVersion>(data.can_commit));
}

// Write a SpecialVersion record to the output trace
void TraceWriter::emitSpecialVersion(const shared_ptr<SpecialVersion>& v) noexcept {
  emitRecord<RecordType::SpecialVersion>(v->canCommit());
}

/********** Process an input trace **********/

void TraceReader::sendTo(IRSink& sink) noexcept {
  while (!done()) {
    // Handle the next record
    switch (peek()) {
      case RecordType::Start:
        handleRecord<RecordType::Start>(sink);
        break;

      case RecordType::Finish:
        handleRecord<RecordType::Finish>(sink);
        break;

      case RecordType::SpecialRef:
        handleRecord<RecordType::SpecialRef>(sink);
        break;

      case RecordType::PipeRef:
        handleRecord<RecordType::PipeRef>(sink);
        break;

      case RecordType::FileRef:
        handleRecord<RecordType::FileRef>(sink);
        break;

      case RecordType::SymlinkRef:
        handleRecord<RecordType::SymlinkRef>(sink);
        break;

      case RecordType::DirRef:
        handleRecord<RecordType::DirRef>(sink);
        break;

      case RecordType::PathRef:
        handleRecord<RecordType::PathRef>(sink);
        break;

      case RecordType::UsingRef:
        handleRecord<RecordType::UsingRef>(sink);
        break;

      case RecordType::DoneWithRef:
        handleRecord<RecordType::DoneWithRef>(sink);
        break;

      case RecordType::CompareRefs:
        handleRecord<RecordType::CompareRefs>(sink);
        break;

      case RecordType::ExpectResult:
        handleRecord<RecordType::ExpectResult>(sink);
        break;

      case RecordType::MatchMetadata:
        handleRecord<RecordType::MatchMetadata>(sink);
        break;

      case RecordType::MatchContent:
        handleRecord<RecordType::MatchContent>(sink);
        break;

      case RecordType::UpdateMetadata:
        handleRecord<RecordType::UpdateMetadata>(sink);
        break;

      case RecordType::UpdateContent:
        handleRecord<RecordType::UpdateContent>(sink);
        break;

      case RecordType::AddEntry:
        handleRecord<RecordType::AddEntry>(sink);
        break;

      case RecordType::RemoveEntry:
        handleRecord<RecordType::RemoveEntry>(sink);
        break;

      case RecordType::Launch:
        handleRecord<RecordType::Launch>(sink);
        break;

      case RecordType::Join:
        handleRecord<RecordType::Join>(sink);
        break;

      case RecordType::Exit:
        handleRecord<RecordType::Exit>(sink);
        break;

      case RecordType::Command:
        handleRecord<RecordType::Command>(sink);
        break;

      case RecordType::String:
        handleRecord<RecordType::String>(sink);
        break;

      case RecordType::NewStrtab:
        handleRecord<RecordType::NewStrtab>(sink);
        break;

      case RecordType::FileVersion:
        handleRecord<RecordType::FileVersion>(sink);
        break;

      case RecordType::SymlinkVersion:
        handleRecord<RecordType::SymlinkVersion>(sink);
        break;

      case RecordType::DirListVersion:
        handleRecord<RecordType::DirListVersion>(sink);
        break;

      case RecordType::PipeWriteVersion:
        handleRecord<RecordType::PipeWriteVersion>(sink);
        break;

      case RecordType::PipeCloseVersion:
        handleRecord<RecordType::PipeCloseVersion>(sink);
        break;

      case RecordType::PipeReadVersion:
        handleRecord<RecordType::PipeReadVersion>(sink);
        break;

      case RecordType::SpecialVersion:
        handleRecord<RecordType::SpecialVersion>(sink);
        break;
    }
  }
}
