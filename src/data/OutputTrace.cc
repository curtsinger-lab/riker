#include "OutputTrace.hh"

#include <filesystem>
#include <fstream>
#include <map>
#include <memory>

#include <cereal/archives/binary.hpp>

#include "runtime/Command.hh"
#include "runtime/Ref.hh"

using std::map;
using std::ofstream;
using std::shared_ptr;

namespace fs = std::filesystem;

/// Add a SpecialRef IR step to the output trace
void OutputTrace::specialRef(shared_ptr<Command> cmd,
                             SpecialRef entity,
                             Command::RefID output) noexcept {
  _records.emplace_back(new SpecialRefRecord(getCommandID(cmd), entity, output));
}

/// Add a PipeRef IR step to the output trace
void OutputTrace::pipeRef(shared_ptr<Command> cmd,
                          Command::RefID read_end,
                          Command::RefID write_end) noexcept {
  _records.emplace_back(new PipeRefRecord(getCommandID(cmd), read_end, write_end));
}

/// Add a FileRef IR step to the output trace
void OutputTrace::fileRef(shared_ptr<Command> cmd, mode_t mode, Command::RefID output) noexcept {
  _records.emplace_back(new FileRefRecord(getCommandID(cmd), mode, output));
}

/// Add a SymlinkRef IR step to the output trace
void OutputTrace::symlinkRef(shared_ptr<Command> cmd,
                             fs::path target,
                             Command::RefID output) noexcept {
  _records.emplace_back(new SymlinkRefRecord(getCommandID(cmd), target, output));
}

/// Add a DirRef IR step to the output trace
void OutputTrace::dirRef(shared_ptr<Command> cmd, mode_t mode, Command::RefID output) noexcept {
  _records.emplace_back(new DirRefRecord(getCommandID(cmd), mode, output));
}

/// Add a PathRef IR step to the output trace
void OutputTrace::pathRef(shared_ptr<Command> cmd,
                          Command::RefID base,
                          fs::path path,
                          AccessFlags flags,
                          Command::RefID output) noexcept {
  _records.emplace_back(new PathRefRecord(getCommandID(cmd), base, path, flags, output));
}

/// Add an Open IR step to the output trace
void OutputTrace::usingRef(shared_ptr<Command> cmd, Command::RefID ref) noexcept {
  _records.emplace_back(new UsingRefRecord(getCommandID(cmd), ref));
}

/// Add a Cloe IR step to the output trace
void OutputTrace::doneWithRef(shared_ptr<Command> cmd, Command::RefID ref) noexcept {
  _records.emplace_back(new DoneWithRefRecord(getCommandID(cmd), ref));
}

/// Add a CompareRefs IR step to the output trace
void OutputTrace::compareRefs(shared_ptr<Command> cmd,
                              Command::RefID ref1,
                              Command::RefID ref2,
                              RefComparison type) noexcept {
  _records.emplace_back(new CompareRefsRecord(getCommandID(cmd), ref1, ref2, type));
}

/// Add a ExpectResult IR step to the output trace
void OutputTrace::expectResult(shared_ptr<Command> cmd,
                               Scenario scenario,
                               Command::RefID ref,
                               int expected) noexcept {
  _records.emplace_back(new ExpectResultRecord(getCommandID(cmd), scenario, ref, expected));
}

/// Add a MatchMetadata IR step to the output trace
void OutputTrace::matchMetadata(shared_ptr<Command> cmd,
                                Scenario scenario,
                                Command::RefID ref,
                                shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(new MatchMetadataRecord(getCommandID(cmd), scenario, ref, version));
}

/// Add a MatchContent IR step to the output trace
void OutputTrace::matchContent(shared_ptr<Command> cmd,
                               Scenario scenario,
                               Command::RefID ref,
                               shared_ptr<Version> version) noexcept {
  _records.emplace_back(new MatchContentRecord(getCommandID(cmd), scenario, ref, version));
}

/// Add a UpdateMetadata IR step to the output trace
void OutputTrace::updateMetadata(shared_ptr<Command> cmd,
                                 Command::RefID ref,
                                 shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(new UpdateMetadataRecord(getCommandID(cmd), ref, version));
}

/// Add a UpdateContent IR step to the output trace
void OutputTrace::updateContent(shared_ptr<Command> cmd,
                                Command::RefID ref,
                                shared_ptr<Version> version) noexcept {
  _records.emplace_back(new UpdateContentRecord(getCommandID(cmd), ref, version));
}

/// Add an AddEntry IR step to the output trace
void OutputTrace::addEntry(shared_ptr<Command> cmd,
                           Command::RefID dir,
                           fs::path name,
                           Command::RefID target) noexcept {
  _records.emplace_back(new AddEntryRecord(getCommandID(cmd), dir, name, target));
}

/// Add a RemoveEntry IR step to the output trace
void OutputTrace::removeEntry(shared_ptr<Command> cmd,
                              Command::RefID dir,
                              fs::path name,
                              Command::RefID target) noexcept {
  _records.emplace_back(new RemoveEntryRecord(getCommandID(cmd), dir, name, target));
}

/// Add a Launch IR step to the output trace
void OutputTrace::launch(shared_ptr<Command> cmd,
                         shared_ptr<Command> child,
                         list<tuple<Command::RefID, Command::RefID>> refs) noexcept {
  // Add the launched command to the set of commands
  Command::ID child_id = addCommand(child);

  _records.emplace_back(new CommandRecord(child_id, child->getArguments(), child->getInitialFDs(),
                                          child->hasExecuted(), child->getExitStatus()));

  // Create the record for the launch IR step
  _records.emplace_back(new LaunchRecord(getCommandID(cmd), getCommandID(child), refs));
}

/// Add a Join IR step to the output trace
void OutputTrace::join(shared_ptr<Command> cmd,
                       shared_ptr<Command> child,
                       int exit_status) noexcept {
  _records.emplace_back(new JoinRecord(getCommandID(cmd), getCommandID(child), exit_status));
}

/// Add a Exit IR step to the output trace
void OutputTrace::exit(shared_ptr<Command> cmd, int exit_status) noexcept {
  _records.emplace_back(new ExitRecord(getCommandID(cmd), exit_status));
}

void OutputTrace::finish() noexcept {
  ofstream out(_filename, std::ios::binary);
  cereal::BinaryOutputArchive archive(out);

  // Write out the magic number and version
  archive(ArchiveMagic, ArchiveVersion);

  // Write out the list of records
  for (auto& r : _records) {
    archive(r);
  }

  unique_ptr<Record> end(new EndRecord());
  // Mark the end of the trace
  archive(end);
}