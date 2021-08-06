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
#include <sys/types.h>

#include "data/IRBuffer.hh"
#include "data/IRSink.hh"
#include "runtime/Command.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"

namespace fs = std::filesystem;

using std::list;
using std::nullopt;
using std::optional;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

/// Tags to identify each type of record we will write out
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
  ContentVersion = 22,
  String = 23,
  NewStrtab = 24,
};

TraceWriter::TraceWriter(optional<string> path) : _id(IRBuffer::getNextID()), _path(path) {
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

template <typename First, typename... Rest>
struct WriteRecord {
  WriteRecord(First first, Rest... rest) noexcept : _first(first), _rest(rest...) {}
  First _first;
  WriteRecord<Rest...> _rest;
} __attribute__((packed));

template <typename Last>
struct WriteRecord<Last> {
  WriteRecord(Last last) : _last(last) {}
  Last _last;
} __attribute__((packed));

// Write a sequence of values to the trace
template <typename... T>
void TraceWriter::write(T... args) noexcept {
  using R = WriteRecord<T...>;
  R* r = reinterpret_cast<R*>(&_data[_pos]);
  *r = {args...};
  _pos += sizeof(R);
}

// Emit a sequence of bytes to the trace
void TraceWriter::emitBytes(void* src, size_t len) noexcept {
  void* dest = &_data[_pos];
  memcpy(dest, src, len);
  _pos += len;
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
  write(RecordType::Command, c->hasExecuted(), argv_length, initial_fds_length);

  // Write out the argv string IDs
  emitBytes(args.data(), args.size() * sizeof(StringID));

  // Write out the initial FDs
  for (auto [fd, ref] : c->getInitialFDs()) {
    write(fd, ref);
  }
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
    emitContentVersion(v);
  }

  // Return the ID
  v->setID(_id, iter->second);
  return iter->second;
}

void TraceWriter::emitContentVersion(const shared_ptr<ContentVersion>& v) noexcept {
  // TODO
}

void TraceWriter::reserveStrings(size_t n) noexcept {
  ASSERT(n < std::numeric_limits<StringID>::max())
      << "Requested number of strings is larger than the total string table size";

  // Check if the string table will fill before n strings are emitted
  if (std::numeric_limits<StringID>::max() - _strtab.size() < n) {
    // Make room by starting a fresh string table
    write(RecordType::NewStrtab);
    _strtab.clear();
  }
}

TraceWriter::StringID TraceWriter::getStringID(const std::string& str) noexcept {
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
      // Indicate that we are starting a fresh string table
      write(RecordType::NewStrtab);

      // Clear the string table and assign an ID of zero
      _strtab.clear();
      id = 0;
    }

    _strtab.emplace_hint(iter, str, id);

    // Write out the string record
    write(RecordType::String);
    char* p = (char*)&_data[_pos];
    strcpy(p, str.c_str());
    _pos += str.size() + 1;

    return id;
  }
}

TraceWriter::PathID TraceWriter::getPathID(const fs::path& path) noexcept {
  return getStringID(path.string());
}

/// Called when starting a trace. The root command is passed in.
void TraceWriter::start(const shared_ptr<Command>& c) noexcept {
  write(RecordType::Start, getCommandID(c));
}

/// Called when the trace is finished
void TraceWriter::finish() noexcept {
  write(RecordType::Finish);
}

/// Handle a SpecialRef IR step
void TraceWriter::specialRef(const shared_ptr<Command>& c,
                             SpecialRef entity,
                             Ref::ID output) noexcept {
  write(RecordType::SpecialRef, getCommandID(c), entity, output);
}

/// Handle a PipeRef IR step
void TraceWriter::pipeRef(const shared_ptr<Command>& c,
                          Ref::ID read_end,
                          Ref::ID write_end) noexcept {
  write(RecordType::PipeRef, getCommandID(c), read_end, write_end);
}

/// Handle a FileRef IR step
void TraceWriter::fileRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept {
  write(RecordType::FileRef, getCommandID(c), mode, output);
}

/// Handle a SymlinkRef IR step
void TraceWriter::symlinkRef(const shared_ptr<Command>& c,
                             fs::path target,
                             Ref::ID output) noexcept {
  write(RecordType::SymlinkRef, getCommandID(c), target, output);
}

/// Handle a DirRef IR step
void TraceWriter::dirRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept {
  write(RecordType::DirRef, getCommandID(c), mode, output);
}

/// Handle a PathRef IR step
void TraceWriter::pathRef(const shared_ptr<Command>& c,
                          Ref::ID base,
                          fs::path path,
                          AccessFlags flags,
                          Ref::ID output) noexcept {
  write(RecordType::PathRef, getCommandID(c), base, getPathID(path), flags, output);
}

/// Handle a UsingRef IR step
void TraceWriter::usingRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  write(RecordType::UsingRef, getCommandID(c), ref);
}

/// Handle a DoneWithRef IR step
void TraceWriter::doneWithRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  write(RecordType::DoneWithRef, getCommandID(c), ref);
}

/// Handle a CompareRefs IR step
void TraceWriter::compareRefs(const shared_ptr<Command>& c,
                              Ref::ID ref1,
                              Ref::ID ref2,
                              RefComparison type) noexcept {
  write(RecordType::CompareRefs, getCommandID(c), ref1, ref2, type);
}

/// Handle an ExpectResult IR step
void TraceWriter::expectResult(const shared_ptr<Command>& c,
                               Scenario scenario,
                               Ref::ID ref,
                               int8_t expected) noexcept {
  write(RecordType::ExpectResult, getCommandID(c), scenario, ref, expected);
}

/// Handle a MatchMetadata IR step
void TraceWriter::matchMetadata(const shared_ptr<Command>& c,
                                Scenario scenario,
                                Ref::ID ref,
                                MetadataVersion version) noexcept {
  write(RecordType::MatchMetadata, getCommandID(c), scenario, ref, version);
}

/// Handel a MatchContent IR step
void TraceWriter::matchContent(const shared_ptr<Command>& c,
                               Scenario scenario,
                               Ref::ID ref,
                               shared_ptr<ContentVersion> version) noexcept {
  write(RecordType::MatchContent, getCommandID(c), scenario, ref, getContentVersionID(version));
}

/// Handle an UpdateMetadata IR step
void TraceWriter::updateMetadata(const shared_ptr<Command>& c,
                                 Ref::ID ref,
                                 MetadataVersion version) noexcept {
  write(RecordType::UpdateMetadata, getCommandID(c), ref, version);
}

/// Handle an UpdateContent IR step
void TraceWriter::updateContent(const shared_ptr<Command>& c,
                                Ref::ID ref,
                                shared_ptr<ContentVersion> version) noexcept {
  write(RecordType::UpdateContent, getCommandID(c), ref, getContentVersionID(version));
}

/// Handle an AddEntry IR step
void TraceWriter::addEntry(const shared_ptr<Command>& c,
                           Ref::ID dir,
                           string name,
                           Ref::ID target) noexcept {
  write(RecordType::AddEntry, getCommandID(c), dir, getStringID(name), target);
}

/// Handle a RemoveEntry IR step
void TraceWriter::removeEntry(const shared_ptr<Command>& c,
                              Ref::ID dir,
                              string name,
                              Ref::ID target) noexcept {
  write(RecordType::RemoveEntry, getCommandID(c), dir, getStringID(name), target);
}

/// Handle a Launch IR step
void TraceWriter::launch(const shared_ptr<Command>& parent,
                         const shared_ptr<Command>& child,
                         list<tuple<Ref::ID, Ref::ID>> refs) noexcept {
  // Compute the length of the ref mapping list, which should fit in a 16-bit integer
  uint16_t refs_length = refs.size();

  // Emit the fixed-length portion of the record
  write(RecordType::Launch, getCommandID(parent), getCommandID(child), refs_length);

  // Now emit the ref mappings
  for (auto [a, b] : refs) {
    write(a, b);
  }
}

/// Handle a Join IR step
void TraceWriter::join(const shared_ptr<Command>& parent,
                       const shared_ptr<Command>& child,
                       int exit_status) noexcept {
  write(RecordType::Join, getCommandID(parent), getCommandID(child), exit_status);
}

/// Handle an Exit IR step
void TraceWriter::exit(const shared_ptr<Command>& c, int exit_status) noexcept {
  write(RecordType::Exit, getCommandID(c), exit_status);
}
