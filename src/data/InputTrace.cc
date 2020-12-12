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
#include "runtime/Run.hh"
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
  handler.specialRef(no_cmd, SpecialRef::stdin, Ref::Stdin);
  handler.usingRef(no_cmd, Ref::Stdin);

  // Create a reference to stdout
  handler.specialRef(no_cmd, SpecialRef::stdout, Ref::Stdout);
  handler.usingRef(no_cmd, Ref::Stdout);

  // Create a reference to stderr
  handler.specialRef(no_cmd, SpecialRef::stderr, Ref::Stderr);
  handler.usingRef(no_cmd, Ref::Stderr);

  // Set up the reference to the root directory
  handler.specialRef(no_cmd, SpecialRef::root, Ref::Root);
  handler.usingRef(no_cmd, Ref::Root);

  // Set up the reference to the working directory
  handler.specialRef(no_cmd, SpecialRef::cwd, Ref::Cwd);
  handler.usingRef(no_cmd, Ref::Cwd);

  // Set up the reference to the launch executable
  handler.specialRef(no_cmd, SpecialRef::launch_exe, Ref::Exe);
  handler.usingRef(no_cmd, Ref::Exe);

  // Create a root command
  Command::ID root_cmd_id = 1;
  auto cmd_args = vector<string>{"dodo-launch"};
  cmd_args.insert(cmd_args.end(), _args.begin(), _args.end());
  auto root_command = make_shared<Command>(cmd_args);

  // Add initial FDs to the root command
  root_command->addInitialFD(STDIN_FILENO, Ref::Stdin);
  root_command->addInitialFD(STDOUT_FILENO, Ref::Stdout);
  root_command->addInitialFD(STDERR_FILENO, Ref::Stderr);

  // Record the root command in the input trace
  addCommand(root_cmd_id, root_command);

  // Create a mapping for references inherited by the root command
  list<tuple<Ref::ID, Ref::ID>> refs = {{Ref::Stdin, Ref::Stdin},   {Ref::Stdout, Ref::Stdout},
                                        {Ref::Stderr, Ref::Stderr}, {Ref::Root, Ref::Root},
                                        {Ref::Cwd, Ref::Cwd},       {Ref::Exe, Ref::Exe}};

  // Launch the root command
  handler.launch(no_cmd, getCommand(root_cmd_id), refs);
}
