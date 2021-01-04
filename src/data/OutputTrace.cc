#include "OutputTrace.hh"

#include <filesystem>
#include <fstream>
#include <map>
#include <memory>

#include <cereal/archives/binary.hpp>

#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "versions/MetadataVersion.hh"

using std::make_unique;
using std::map;
using std::ofstream;
using std::shared_ptr;

namespace fs = std::filesystem;

// Create a trace at the given path
OutputTrace::OutputTrace(string filename) noexcept :
    _out(filename, std::ios::binary), _archive(_out) {
  // Write out the magic number and version
  _archive(ArchiveMagic, ArchiveVersion);

  // Add the null command to the command map with ID 0
  _commands.emplace(Command::getNullCommand(), 0);
}

/// Add a SpecialRef IR step to the output trace
void OutputTrace::specialRef(const shared_ptr<Command>& cmd,
                             SpecialRef entity,
                             Ref::ID output) noexcept {
  _archive(unique_ptr<Record>(make_unique<SpecialRefRecord>(getCommandID(cmd), entity, output)));
}

/// Add a PipeRef IR step to the output trace
void OutputTrace::pipeRef(const shared_ptr<Command>& cmd,
                          Ref::ID read_end,
                          Ref::ID write_end) noexcept {
  _archive(unique_ptr<Record>(make_unique<PipeRefRecord>(getCommandID(cmd), read_end, write_end)));
}

/// Add a FileRef IR step to the output trace
void OutputTrace::fileRef(const shared_ptr<Command>& cmd, mode_t mode, Ref::ID output) noexcept {
  _archive(unique_ptr<Record>(make_unique<FileRefRecord>(getCommandID(cmd), mode, output)));
}

/// Add a SymlinkRef IR step to the output trace
void OutputTrace::symlinkRef(const shared_ptr<Command>& cmd,
                             fs::path target,
                             Ref::ID output) noexcept {
  _archive(unique_ptr<Record>(make_unique<SymlinkRefRecord>(getCommandID(cmd), target, output)));
}

/// Add a DirRef IR step to the output trace
void OutputTrace::dirRef(const shared_ptr<Command>& cmd, mode_t mode, Ref::ID output) noexcept {
  _archive(unique_ptr<Record>(make_unique<DirRefRecord>(getCommandID(cmd), mode, output)));
}

/// Add a PathRef IR step to the output trace
void OutputTrace::pathRef(const shared_ptr<Command>& cmd,
                          Ref::ID base,
                          fs::path path,
                          AccessFlags flags,
                          Ref::ID output) noexcept {
  _archive(
      unique_ptr<Record>(make_unique<PathRefRecord>(getCommandID(cmd), base, path, flags, output)));
}

/// Add an Open IR step to the output trace
void OutputTrace::usingRef(const shared_ptr<Command>& cmd, Ref::ID ref) noexcept {
  _archive(unique_ptr<Record>(make_unique<UsingRefRecord>(getCommandID(cmd), ref)));
}

/// Add a Cloe IR step to the output trace
void OutputTrace::doneWithRef(const shared_ptr<Command>& cmd, Ref::ID ref) noexcept {
  _archive(unique_ptr<Record>(make_unique<DoneWithRefRecord>(getCommandID(cmd), ref)));
}

/// Add a CompareRefs IR step to the output trace
void OutputTrace::compareRefs(const shared_ptr<Command>& cmd,
                              Ref::ID ref1,
                              Ref::ID ref2,
                              RefComparison type) noexcept {
  _archive(unique_ptr<Record>(make_unique<CompareRefsRecord>(getCommandID(cmd), ref1, ref2, type)));
}

/// Add a ExpectResult IR step to the output trace
void OutputTrace::expectResult(const shared_ptr<Command>& cmd,
                               Scenario scenario,
                               Ref::ID ref,
                               int expected) noexcept {
  _archive(unique_ptr<Record>(
      make_unique<ExpectResultRecord>(getCommandID(cmd), scenario, ref, expected)));
}

/// Add a MatchMetadata IR step to the output trace
void OutputTrace::matchMetadata(const shared_ptr<Command>& cmd,
                                Scenario scenario,
                                Ref::ID ref,
                                shared_ptr<MetadataVersion> version) noexcept {
  _archive(unique_ptr<Record>(
      make_unique<MatchMetadataRecord>(getCommandID(cmd), scenario, ref, version)));
}

/// Add a MatchContent IR step to the output trace
void OutputTrace::matchContent(const shared_ptr<Command>& cmd,
                               Scenario scenario,
                               Ref::ID ref,
                               shared_ptr<Version> version) noexcept {
  // Preserve this version in the cache
  if (version->isCached()) version->gcLink();

  _archive(unique_ptr<Record>(
      make_unique<MatchContentRecord>(getCommandID(cmd), scenario, ref, version)));
}

/// Add a UpdateMetadata IR step to the output trace
void OutputTrace::updateMetadata(const shared_ptr<Command>& cmd,
                                 Ref::ID ref,
                                 shared_ptr<MetadataVersion> version) noexcept {
  _archive(unique_ptr<Record>(make_unique<UpdateMetadataRecord>(getCommandID(cmd), ref, version)));
}

/// Add a UpdateContent IR step to the output trace
void OutputTrace::updateContent(const shared_ptr<Command>& cmd,
                                Ref::ID ref,
                                shared_ptr<Version> version) noexcept {
  // Preserve this version in the cache
  if (version->isCached()) version->gcLink();

  _archive(unique_ptr<Record>(make_unique<UpdateContentRecord>(getCommandID(cmd), ref, version)));
}

/// Add an AddEntry IR step to the output trace
void OutputTrace::addEntry(const shared_ptr<Command>& cmd,
                           Ref::ID dir,
                           fs::path name,
                           Ref::ID target) noexcept {
  _archive(unique_ptr<Record>(make_unique<AddEntryRecord>(getCommandID(cmd), dir, name, target)));
}

/// Add a RemoveEntry IR step to the output trace
void OutputTrace::removeEntry(const shared_ptr<Command>& cmd,
                              Ref::ID dir,
                              fs::path name,
                              Ref::ID target) noexcept {
  _archive(
      unique_ptr<Record>(make_unique<RemoveEntryRecord>(getCommandID(cmd), dir, name, target)));
}

/// Add a Launch IR step to the output trace
void OutputTrace::launch(const shared_ptr<Command>& cmd,
                         const shared_ptr<Command>& child,
                         list<tuple<Ref::ID, Ref::ID>> refs) noexcept {
  // Add the launched command to the set of commands
  Command::ID child_id = addCommand(child);

  _archive(unique_ptr<Record>(make_unique<CommandRecord>(
      child_id, child->getArguments(), child->getInitialFDs(), child->hasExecuted())));

  // Create the record for the launch IR step
  _archive(
      unique_ptr<Record>(make_unique<LaunchRecord>(getCommandID(cmd), getCommandID(child), refs)));
}

/// Add a Join IR step to the output trace
void OutputTrace::join(const shared_ptr<Command>& cmd,
                       const shared_ptr<Command>& child,
                       int exit_status) noexcept {
  _archive(unique_ptr<Record>(
      make_unique<JoinRecord>(getCommandID(cmd), getCommandID(child), exit_status)));
}

/// Add a Exit IR step to the output trace
void OutputTrace::exit(const shared_ptr<Command>& cmd, int exit_status) noexcept {
  _archive(unique_ptr<Record>(make_unique<ExitRecord>(getCommandID(cmd), exit_status)));
}

void OutputTrace::finish() noexcept {
  // Mark the end of the trace
  _archive(unique_ptr<Record>(make_unique<EndRecord>()));

  // Close the output filestream
  _out.close();
}