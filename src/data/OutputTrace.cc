#include "OutputTrace.hh"

#include <filesystem>
#include <fstream>
#include <map>
#include <memory>

#include <cereal/archives/binary.hpp>

#include "runtime/Command.hh"
#include "runtime/RefResult.hh"

using std::map;
using std::ofstream;
using std::shared_ptr;

namespace fs = std::filesystem;

/// Add a SpecialRef IR step to the output trace
void OutputTrace::specialRef(shared_ptr<Command> cmd,
                             SpecialRef entity,
                             shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new SpecialRefRecord(getCommandID(cmd), entity, getRefResultID(output)));
}

/// Add a PipeRef IR step to the output trace
void OutputTrace::pipeRef(shared_ptr<Command> cmd,
                          shared_ptr<RefResult> read_end,
                          shared_ptr<RefResult> write_end) noexcept {
  _records.emplace_back(
      new PipeRefRecord(getCommandID(cmd), getRefResultID(read_end), getRefResultID(write_end)));
}

/// Add a FileRef IR step to the output trace
void OutputTrace::fileRef(shared_ptr<Command> cmd,
                          mode_t mode,
                          shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new FileRefRecord(getCommandID(cmd), mode, getRefResultID(output)));
}

/// Add a SymlinkRef IR step to the output trace
void OutputTrace::symlinkRef(shared_ptr<Command> cmd,
                             fs::path target,
                             shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new SymlinkRefRecord(getCommandID(cmd), target, getRefResultID(output)));
}

/// Add a DirRef IR step to the output trace
void OutputTrace::dirRef(shared_ptr<Command> cmd,
                         mode_t mode,
                         shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new DirRefRecord(getCommandID(cmd), mode, getRefResultID(output)));
}

/// Add a PathRef IR step to the output trace
void OutputTrace::pathRef(shared_ptr<Command> cmd,
                          shared_ptr<RefResult> base,
                          fs::path path,
                          AccessFlags flags,
                          shared_ptr<RefResult> output) noexcept {
  _records.emplace_back(new PathRefRecord(getCommandID(cmd), getRefResultID(base), path, flags,
                                          getRefResultID(output)));
}

/// Add an Open IR step to the output trace
void OutputTrace::usingRef(shared_ptr<Command> cmd, shared_ptr<RefResult> ref) noexcept {
  _records.emplace_back(new UsingRefRecord(getCommandID(cmd), getRefResultID(ref)));
}

/// Add a Cloe IR step to the output trace
void OutputTrace::doneWithRef(shared_ptr<Command> cmd, shared_ptr<RefResult> ref) noexcept {
  _records.emplace_back(new DoneWithRefRecord(getCommandID(cmd), getRefResultID(ref)));
}

/// Add a CompareRefs IR step to the output trace
void OutputTrace::compareRefs(shared_ptr<Command> cmd,
                              shared_ptr<RefResult> ref1,
                              shared_ptr<RefResult> ref2,
                              RefComparison type) noexcept {
  _records.emplace_back(
      new CompareRefsRecord(getCommandID(cmd), getRefResultID(ref1), getRefResultID(ref2), type));
}

/// Add a ExpectResult IR step to the output trace
void OutputTrace::expectResult(shared_ptr<Command> cmd,
                               Scenario scenario,
                               shared_ptr<RefResult> ref,
                               int expected) noexcept {
  _records.emplace_back(
      new ExpectResultRecord(getCommandID(cmd), scenario, getRefResultID(ref), expected));
}

/// Add a MatchMetadata IR step to the output trace
void OutputTrace::matchMetadata(shared_ptr<Command> cmd,
                                Scenario scenario,
                                shared_ptr<RefResult> ref,
                                shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(
      new MatchMetadataRecord(getCommandID(cmd), scenario, getRefResultID(ref), version));
}

/// Add a MatchContent IR step to the output trace
void OutputTrace::matchContent(shared_ptr<Command> cmd,
                               Scenario scenario,
                               shared_ptr<RefResult> ref,
                               shared_ptr<Version> version) noexcept {
  _records.emplace_back(
      new MatchContentRecord(getCommandID(cmd), scenario, getRefResultID(ref), version));
}

/// Add a UpdateMetadata IR step to the output trace
void OutputTrace::updateMetadata(shared_ptr<Command> cmd,
                                 shared_ptr<RefResult> ref,
                                 shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(new UpdateMetadataRecord(getCommandID(cmd), getRefResultID(ref), version));
}

/// Add a UpdateContent IR step to the output trace
void OutputTrace::updateContent(shared_ptr<Command> cmd,
                                shared_ptr<RefResult> ref,
                                shared_ptr<Version> version) noexcept {
  _records.emplace_back(new UpdateContentRecord(getCommandID(cmd), getRefResultID(ref), version));
}

/// Add an AddEntry IR step to the output trace
void OutputTrace::addEntry(shared_ptr<Command> cmd,
                           shared_ptr<RefResult> dir,
                           fs::path name,
                           shared_ptr<RefResult> target) noexcept {
  _records.emplace_back(
      new AddEntryRecord(getCommandID(cmd), getRefResultID(dir), name, getRefResultID(target)));
}

/// Add a RemoveEntry IR step to the output trace
void OutputTrace::removeEntry(shared_ptr<Command> cmd,
                              shared_ptr<RefResult> dir,
                              fs::path name,
                              shared_ptr<RefResult> target) noexcept {
  _records.emplace_back(
      new RemoveEntryRecord(getCommandID(cmd), getRefResultID(dir), name, getRefResultID(target)));
}

/// Add a Launch IR step to the output trace
void OutputTrace::launch(shared_ptr<Command> cmd, shared_ptr<Command> child) noexcept {
  // Add the launched command to the set of commands
  Command::ID child_id = addCommand(child);
  RefResult::ID root_id = getRefResultID(child->getInitialRootDir());
  RefResult::ID cwd_id = getRefResultID(child->getInitialWorkingDir());
  RefResult::ID exe_id = getRefResultID(child->getExecutable());

  map<int, RefResult::ID> fds;
  for (auto [fd, info] : child->getInitialFDs()) {
    fds[fd] = getRefResultID(info.getRef());
  }

  _records.emplace_back(new CommandRecord(child_id, root_id, cwd_id, exe_id, child->getArguments(),
                                          fds, child->hasExecuted(), child->getExitStatus()));

  // Create the record for the launch IR step
  _records.emplace_back(new LaunchRecord(getCommandID(cmd), getCommandID(child)));
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