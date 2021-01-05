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
#include "data/DefaultTrace.hh"
#include "data/Record.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/CommandRun.hh"
#include "runtime/Ref.hh"
#include "runtime/env.hh"
#include "util/log.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::endl;
using std::ifstream;
using std::make_shared;
using std::make_unique;
using std::map;
using std::ostream;
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
  _commands.emplace_back(Command::getNullCommand());
}

unique_ptr<IRSource> InputTrace::load(string filename, vector<string> args) noexcept {
  try {
    // Try to create an input trace and return it
    unique_ptr<InputTrace> result(new InputTrace(filename, args));
    return result;

  } catch (cereal::Exception& e) {
    // If there is an exception when loading the trace, revert to a default trace
    return make_unique<DefaultTrace>(args);
  }
}

// Run this trace
void InputTrace::sendTo(IRSink& handler) noexcept {
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
