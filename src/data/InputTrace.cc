#include "InputTrace.hh"

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
#include "data/Record.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/Env.hh"
#include "runtime/Ref.hh"
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

// Run this trace
void InputTrace::sendTo(TraceHandler& handler) noexcept {
  // Reset all the commands in this input trace
  for (const auto& cmd : _commands) {
    cmd->reset();
  }

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

  // Create a reference to stdin
  handler.specialRef(no_cmd, SpecialRef::stdin, Command::StdinRef);
  handler.usingRef(no_cmd, Command::StdinRef);

  // Create a reference to stdout
  handler.specialRef(no_cmd, SpecialRef::stdout, Command::StdoutRef);
  handler.usingRef(no_cmd, Command::StdoutRef);

  // Create a reference to stderr
  handler.specialRef(no_cmd, SpecialRef::stderr, Command::StderrRef);
  handler.usingRef(no_cmd, Command::StderrRef);

  // Set up the reference to the root directory
  handler.specialRef(no_cmd, SpecialRef::root, Command::RootRef);
  handler.usingRef(no_cmd, Command::RootRef);

  // Set up the reference to the working directory
  handler.specialRef(no_cmd, SpecialRef::cwd, Command::CwdRef);
  handler.usingRef(no_cmd, Command::CwdRef);

  // Set up the reference to the launch executable
  handler.specialRef(no_cmd, SpecialRef::launch_exe, Command::ExeRef);
  handler.usingRef(no_cmd, Command::ExeRef);

  // Create a root command
  Command::ID root_cmd_id = 1;
  auto cmd_args = vector<string>{"dodo-launch"};
  cmd_args.insert(cmd_args.end(), _args.begin(), _args.end());
  auto root_command = make_shared<Command>(cmd_args);

  // Add initial FDs to the root command
  root_command->addInitialFD(STDIN_FILENO, Command::StdinRef);
  root_command->addInitialFD(STDOUT_FILENO, Command::StdoutRef);
  root_command->addInitialFD(STDERR_FILENO, Command::StderrRef);

  // Record the root command in the input trace
  addCommand(root_cmd_id, root_command);

  // Create a mapping for references inherited by the root command
  list<tuple<Command::RefID, Command::RefID>> refs = {
      {Command::StdinRef, Command::StdinRef},   {Command::StdoutRef, Command::StdoutRef},
      {Command::StderrRef, Command::StderrRef}, {Command::RootRef, Command::RootRef},
      {Command::CwdRef, Command::CwdRef},       {Command::ExeRef, Command::ExeRef}};

  // Launch the root command
  handler.launch(no_cmd, getCommand(root_cmd_id), refs);
}
