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

TraceWriter::TraceWriter(optional<string> filename) : _id(IRBuffer::getNextID()) {
  // Was a filename provided?
  if (filename.has_value()) {
    // Create a file to hold the trace
    _fd = open(filename.value().c_str(), O_RDWR | O_CREAT | O_TRUNC, 0644);
    FAIL_IF(_fd == -1) << "Failed to open file " << filename.value() << ": " << ERR;

  } else {
    // Create a file to hold the trace
    _fd = open("/tmp", O_RDWR | O_TMPFILE, 0644);
    FAIL_IF(_fd == -1) << "Failed to open temporary file: " << ERR;
  }

  // Extend the trace to 256MB. Eventually it needs to grow automatically
  _length = 256 * 1024 * 1024;
  int rc = ftruncate(_fd, _length);
  FAIL_IF(rc != 0) << "Failed to extend file to 256MB: " << ERR;

  // Map the file
  _data = (uint8_t*)mmap(nullptr, _length, PROT_READ | PROT_WRITE, MAP_SHARED, _fd, 0);
  FAIL_IF(_data == MAP_FAILED) << "Failed to mmap file: " << ERR;
}

TraceWriter::~TraceWriter() noexcept {
  int rc = ftruncate(_fd, _pos);
  WARN_IF(rc != 0) << "Failed to truncate trace: " << ERR;
  rc = close(_fd);
  WARN_IF(rc != 0) << "Failed to close trace: " << ERR;
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

Command::ID TraceWriter::getCommandID(const std::shared_ptr<Command>& c) noexcept {
  auto id = c->getID(_id);
  if (id.has_value()) return id.value();

  // Look for the provided command in the map of known commands
  auto iter = _commands.find(c);
  if (iter == _commands.end()) {
    // The command was not found. Add it now
    Command::ID id = _commands.size();
    iter = _commands.emplace_hint(iter, c, id);

    // Emit each of the strings in the argv array
    vector<StringID> args;
    // TODO: make sure we won't get a new string table in the middle of this process
    for (const auto& arg : c->getArguments()) {
      args.push_back(getStringID(arg));
    }

    // Write out the fixed-length portion of the command record
    write(RecordType::Command, c->hasExecuted(), static_cast<uint16_t>(args.size()),
          static_cast<uint16_t>(c->getInitialFDs().size()));

    // Write out the argv string IDs
    for (auto id : args) {
      write(id);
    }
  }

  c->setID(_id, iter->second);
  return iter->second;
}

// Get the ID for a content version. Emit a new metadata version record if necessary
ContentVersion::ID TraceWriter::getContentVersionID(const shared_ptr<ContentVersion>& cv) noexcept {
  auto id = cv->getID(_id);
  if (id.has_value()) return id.value();

  // Look for the provided content version in the map of known versions
  auto iter = _versions.find(cv);
  if (iter == _versions.end()) {
    // If the version wasn't found, add it now
    ContentVersion::ID id = _versions.size();
    iter = _versions.emplace_hint(iter, cv, id);

    // TODO: write out the content version record
  }

  // Return the ID
  cv->setID(_id, iter->second);
  return iter->second;
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
  // TODO: write list of ref mappings
  write(RecordType::Launch, getCommandID(parent), getCommandID(child));
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
