#include "InputTrace.hh"

#include <cstddef>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include <cereal/archives/binary.hpp>

#include "data/DefaultTrace.hh"
#include "data/Record.hh"
#include "runtime/Command.hh"
#include "util/log.hh"

using std::make_unique;
using std::map;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::unique_ptr;
using std::vector;

InputTrace::InputTrace(string filename, vector<string> args) :
    _input(filename, std::ios::binary), _archive(_input), _args(args) {
  // Load the version header from the trace file
  size_t magic;
  size_t version;
  _archive(magic, version);

  // Check the magic number and version
  if (magic != ArchiveMagic) {
    WARN << "Saved trace does appears to be invalid. Running a full build.";
    throw cereal::Exception("Wrong magic number");

  } else if (version != ArchiveVersion) {
    WARN << "Saved trace is not the correct version. Running a full build.";
    throw cereal::Exception("Wrong version");
  }

  // Add the null command to the command map
  _commands.emplace_back(Command::createEmptyCommand());
}

tuple<shared_ptr<Command>, unique_ptr<IRSource>> InputTrace::load(string filename,
                                                                  vector<string> args) noexcept {
  try {
    // Try to create an input trace and return it
    unique_ptr<InputTrace> trace(new InputTrace(filename, args));
    return {trace->getRootCommand(), std::move(trace)};

  } catch (cereal::Exception& e) {
    // If there is an exception when loading the trace, revert to a default trace
    auto trace = make_unique<DefaultTrace>(args);
    return {trace->getRootCommand(), std::move(trace)};
  }
}

// Run this trace
void InputTrace::sendTo(IRSink& handler) noexcept {
  // Send the root command
  handler.start(getCommand(0));

  // Loop until we hit the end of the trace
  bool done = false;
  while (!done) {
    unique_ptr<Record> record;
    _archive(record);
    done = record->isEnd();
    record->handle(*this, handler);
  }

  handler.finish();
}

/// Add a command with a known ID to this input trace. If the command ID has already been loaded,
/// the original instance will be used and not the new one.
void InputTrace::addCommand(Command::ID id, shared_ptr<Command> cmd) noexcept {
  // Grow the commands vector if necessary
  if (_commands.size() <= id) _commands.resize(id + 1);

  // If the referenced entry is unset, save the provided cmd
  if (!_commands[id]) _commands[id] = cmd;
}

/// Get a command from its ID
const shared_ptr<Command>& InputTrace::getCommand(Command::ID id) const noexcept {
  return _commands[id];
}

/// Add a MetadataVersion with a known ID to this input trace
void InputTrace::addMetadataVersion(MetadataVersion::ID id,
                                    shared_ptr<MetadataVersion> mv) noexcept {
  // Grow the vector if necessary
  if (_metadata_versions.size() <= id) _metadata_versions.resize(id + 1);

  // If the referenced entry is not set, save the provided version
  if (!_metadata_versions[id]) _metadata_versions[id] = mv;
}

/// Get a metadata version from its ID
const shared_ptr<MetadataVersion>& InputTrace::getMetadataVersion(
    MetadataVersion::ID id) const noexcept {
  return _metadata_versions[id];
}

/// Add a ContentVersion with a known ID to this input trace
void InputTrace::addContentVersion(ContentVersion::ID id, shared_ptr<ContentVersion> cv) noexcept {
  // Grow the vector if necessary
  if (_content_versions.size() <= id) _content_versions.resize(id + 1);

  // If the referenced entry is not set, save the provided version
  if (!_content_versions[id]) _content_versions[id] = cv;
}

/// Get a content version from its ID
const shared_ptr<ContentVersion>& InputTrace::getContentVersion(
    ContentVersion::ID id) const noexcept {
  return _content_versions[id];
}
