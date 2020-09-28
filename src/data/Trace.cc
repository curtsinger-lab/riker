#include "Trace.hh"

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include <fcntl.h>
#include <sys/stat.h>

#include <cereal/archives/binary.hpp>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "data/AccessFlags.hh"
#include "data/FileDescriptor.hh"
#include "data/Record.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/Env.hh"
#include "runtime/RefResult.hh"
#include "util/log.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"
#include "versions/Version.hh"

using std::endl;
using std::make_shared;
using std::make_unique;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

enum : size_t { ArchiveMagic = 0xD0D0D035178357, ArchiveVersion = 101 };

// Run this trace
void InputTrace::sendTo(TraceHandler& handler) noexcept {
  bool use_default = false;

  try {
    // Open the file for reading. Must pass std::ios::binary!
    ifstream f(_filename, std::ios::binary);

    // Initialize cereal's binary archive reader
    cereal::BinaryInputArchive archive(f);

    // Load the version header from the trace file
    size_t magic;
    size_t version;
    archive(magic, version);

    // Check the magic number and version
    if (magic != ArchiveMagic) {
      WARN << "Saved trace does appears to be invalid. Running a full build.";
      use_default = true;
    } else if (version != ArchiveVersion) {
      WARN << "Saved trace is not the correct version. Running a full build.";
      use_default = true;
    }

    // If we are not using a default trace, load it now
    if (!use_default) {
      // Loop until we hit the end of the trace
      bool done = false;
      while (!done) {
        unique_ptr<Record> record;
        archive(record);
        done = record->isEnd();
        record->handle(*this, handler);
      }
    }

  } catch (cereal::Exception& e) {
    // If there is an exception when loading the trace, revert to a default trace
    use_default = true;
  }

  if (use_default) sendDefault(handler);

  handler.finish();
}

void InputTrace::sendDefault(TraceHandler& handler) noexcept {
  Command::ID no_cmd_id = 0;
  auto no_cmd = getCommand(no_cmd_id);

  // Create the initial pipe references
  RefResult::ID stdin_ref_id = 0;
  auto stdin_ref = getRefResult(stdin_ref_id);
  handler.specialRef(no_cmd, SpecialRef::stdin, stdin_ref);

  RefResult::ID stdout_ref_id = 1;
  auto stdout_ref = getRefResult(stdout_ref_id);
  handler.specialRef(no_cmd, SpecialRef::stdout, stdout_ref);

  RefResult::ID stderr_ref_id = 2;
  auto stderr_ref = getRefResult(stderr_ref_id);
  handler.specialRef(no_cmd, SpecialRef::stderr, stderr_ref);

  // Create a reference to the root directory
  RefResult::ID root_ref_id = 3;
  auto root_ref = getRefResult(root_ref_id);
  handler.specialRef(no_cmd, SpecialRef::root, root_ref);

  // Create a reference to the current working directory and add it to the trace
  RefResult::ID cwd_ref_id = 4;
  auto cwd_ref = getRefResult(cwd_ref_id);
  handler.specialRef(no_cmd, SpecialRef::cwd, cwd_ref);

  // Set up the reference to the dodo-launch executable and add it to the trace
  RefResult::ID exe_ref_id = 5;
  auto exe_ref = getRefResult(exe_ref_id);
  handler.specialRef(no_cmd, SpecialRef::launch_exe, exe_ref);

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(stdin_ref, AccessFlags{.r = true})},
                                  {1, FileDescriptor(stdout_ref, AccessFlags{.w = true})},
                                  {2, FileDescriptor(stderr_ref, AccessFlags{.w = true})}};

  // Create a root command
  Command::ID root_cmd_id = 1;
  addCommand(root_cmd_id,
             make_shared<Command>(exe_ref, vector<string>{"dodo-launch"}, fds, cwd_ref, root_ref));

  // Launch the root command
  handler.launch(no_cmd, getCommand(root_cmd_id));
}

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

/// Add a ExpectResult IR step to the output trace
void OutputTrace::expectResult(shared_ptr<Command> cmd,
                               shared_ptr<RefResult> ref,
                               int expected) noexcept {
  _records.emplace_back(new ExpectResultRecord(getCommandID(cmd), getRefResultID(ref), expected));
}

/// Add a MatchMetadata IR step to the output trace
void OutputTrace::matchMetadata(shared_ptr<Command> cmd,
                                shared_ptr<RefResult> ref,
                                shared_ptr<MetadataVersion> version) noexcept {
  _records.emplace_back(new MatchMetadataRecord(getCommandID(cmd), getRefResultID(ref), version));
}

/// Add a MatchContent IR step to the output trace
void OutputTrace::matchContent(shared_ptr<Command> cmd,
                               shared_ptr<RefResult> ref,
                               shared_ptr<Version> version) noexcept {
  _records.emplace_back(new MatchContentRecord(getCommandID(cmd), getRefResultID(ref), version));
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

  map<int, tuple<RefResult::ID, AccessFlags>> fds;
  for (auto [fd, info] : child->getInitialFDs()) {
    fds[fd] = {getRefResultID(info.getRef()), info.getFlags()};
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
