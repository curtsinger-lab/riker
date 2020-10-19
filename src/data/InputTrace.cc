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
  handler.open(no_cmd, stdin_ref);

  RefResult::ID stdout_ref_id = 1;
  auto stdout_ref = getRefResult(stdout_ref_id);
  handler.specialRef(no_cmd, SpecialRef::stdout, stdout_ref);
  handler.open(no_cmd, stdout_ref);

  RefResult::ID stderr_ref_id = 2;
  auto stderr_ref = getRefResult(stderr_ref_id);
  handler.specialRef(no_cmd, SpecialRef::stderr, stderr_ref);
  handler.open(no_cmd, stderr_ref);

  // Create a reference to the root directory
  RefResult::ID root_ref_id = 3;
  auto root_ref = getRefResult(root_ref_id);
  handler.specialRef(no_cmd, SpecialRef::root, root_ref);
  handler.open(no_cmd, root_ref);

  // Create a reference to the current working directory and add it to the trace
  RefResult::ID cwd_ref_id = 4;
  auto cwd_ref = getRefResult(cwd_ref_id);
  handler.specialRef(no_cmd, SpecialRef::cwd, cwd_ref);
  handler.open(no_cmd, cwd_ref);

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
  auto cmd_args = vector<string>{"dodo-launch"};
  cmd_args.insert(cmd_args.end(), _args.begin(), _args.end());
  addCommand(root_cmd_id, make_shared<Command>(exe_ref, cmd_args, fds, cwd_ref, root_ref));

  // Launch the root command
  handler.launch(no_cmd, getCommand(root_cmd_id));
}
