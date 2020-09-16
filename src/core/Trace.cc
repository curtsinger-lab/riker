#include "Trace.hh"

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/stat.h>

#include <cereal/archives/binary.hpp>
#include <cereal/types/array.hpp>
#include <cereal/types/list.hpp>
#include <cereal/types/map.hpp>
#include <cereal/types/memory.hpp>
#include <cereal/types/optional.hpp>
#include <cereal/types/polymorphic.hpp>
#include <cereal/types/set.hpp>
#include <cereal/types/tuple.hpp>
#include <cereal/types/vector.hpp>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "build/Build.hh"
#include "build/Env.hh"
#include "core/AccessFlags.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
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
using std::vector;

enum class RecordType : uint8_t {
  SpecialRef,
  PipeRef,
  FileRef,
  SymlinkRef,
  DirRef,
  PathRef,
  ExpectResult,
  MatchMetadata,
  MatchContent,
  UpdateMetadata,
  UpdateContent,
  Launch,
  Join,
  Exit,
  End
};

InputTrace::InputTrace(string filename) noexcept {
  try {
    // Open the file for reading. Must pass std::ios::binary!
    ifstream f(filename, std::ios::binary);

    // Initialize cereal's binary archive reader
    cereal::BinaryInputArchive archive(f);

    // Loop until we hit the end of the trace
    while (true) {
      auto more = readRecord(archive);
      if (!more) break;
    }
  } catch (cereal::Exception& e) {
    // If loading failed, initialize a default trace
    initDefault();
  }
}

bool InputTrace::readRecord(cereal::BinaryInputArchive& archive) {
  // Load the tag for the next record
  RecordType tag;
  archive(tag);

  shared_ptr<Command> command;

  // What kind of record is this?
  if (tag == RecordType::SpecialRef) {
    auto step = make_unique<SpecialRef>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::PipeRef) {
    auto step = make_unique<PipeRef>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::FileRef) {
    auto step = make_unique<FileRef>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::SymlinkRef) {
    auto step = make_unique<SymlinkRef>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::DirRef) {
    auto step = make_unique<DirRef>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::PathRef) {
    auto step = make_unique<PathRef>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::ExpectResult) {
    auto step = make_unique<ExpectResult>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::MatchMetadata) {
    auto step = make_unique<MatchMetadata>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::MatchContent) {
    auto step = make_unique<MatchContent>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::UpdateMetadata) {
    auto step = make_unique<UpdateMetadata>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::UpdateContent) {
    auto step = make_unique<UpdateContent>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::Launch) {
    auto step = make_unique<Launch>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::Join) {
    auto step = make_unique<Join>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::Exit) {
    auto step = make_unique<Exit>();
    archive(command, *step);
    _steps.emplace_back(command, std::move(step));

  } else if (tag == RecordType::End) {
    // There are no more records to read
    return false;

  } else {
    FAIL << "Unknown record type";
  }

  return true;
}

void InputTrace::initDefault() noexcept {
  // Clear the step list
  _steps.clear();

  // Create the initial pipe references
  auto stdin_ref = make_shared<RefResult>();
  _steps.emplace_back(nullptr, make_unique<SpecialRef>(SpecialRef::stdin, stdin_ref));

  auto stdout_ref = make_shared<RefResult>();
  _steps.emplace_back(nullptr, make_unique<SpecialRef>(SpecialRef::stdout, stdout_ref));

  auto stderr_ref = make_shared<RefResult>();
  _steps.emplace_back(nullptr, make_unique<SpecialRef>(SpecialRef::stderr, stderr_ref));

  // Create a reference to the root directory
  auto root_ref = make_shared<RefResult>();
  _steps.emplace_back(nullptr, make_unique<SpecialRef>(SpecialRef::root, root_ref));

  // Create a reference to the current working directory and add it to the trace
  auto cwd_ref = make_shared<RefResult>();
  _steps.emplace_back(nullptr, make_unique<SpecialRef>(SpecialRef::cwd, cwd_ref));

  // Set up the reference to the dodo-launch executable and add it to the trace
  auto exe_ref = make_shared<RefResult>();
  _steps.emplace_back(nullptr, make_unique<SpecialRef>(SpecialRef::launch_exe, exe_ref));

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(stdin_ref, AccessFlags{.r = true})},
                                  {1, FileDescriptor(stdout_ref, AccessFlags{.w = true})},
                                  {2, FileDescriptor(stderr_ref, AccessFlags{.w = true})}};

  // Make a root command
  auto root_cmd =
      make_shared<Command>(exe_ref, vector<string>{"dodo-launch"}, fds, cwd_ref, root_ref);

  // Make a launch action for the root command
  _steps.emplace_back(nullptr, make_unique<Launch>(root_cmd));
}

// Print this trace
ostream& InputTrace::print(ostream& o) const noexcept {
  for (auto& [c, s] : _steps) {
    if (c) {
      o << c << ": " << s << endl;
    } else {
      o << s << endl;
    }
  }
  return o;
}

// Run this trace
shared_ptr<Env> InputTrace::run(Build& build) noexcept {
  for (auto& [command, step] : _steps) {
    build.runStep(command, *step);
  }
  return build.finish();
}

OutputTrace::OutputTrace(string filename) noexcept :
    _output(filename, std::ios::binary), _archive(_output) {}

OutputTrace::~OutputTrace() noexcept {
  // Write out the list of steps
  for (auto& [c, s] : _steps) {
    writeRecord(c, s.get());
  }

  // Mark the end of the trace
  _archive(RecordType::End);
}

void OutputTrace::writeRecord(shared_ptr<Command> command, Step* step) noexcept {
  if (auto v = dynamic_cast<SpecialRef*>(step)) {
    _archive(RecordType::SpecialRef, command, *v);

  } else if (auto v = dynamic_cast<PipeRef*>(step)) {
    _archive(RecordType::PipeRef, command, *v);

  } else if (auto v = dynamic_cast<FileRef*>(step)) {
    _archive(RecordType::FileRef, command, *v);

  } else if (auto v = dynamic_cast<SymlinkRef*>(step)) {
    _archive(RecordType::SymlinkRef, command, *v);

  } else if (auto v = dynamic_cast<DirRef*>(step)) {
    _archive(RecordType::DirRef, command, *v);

  } else if (auto v = dynamic_cast<PathRef*>(step)) {
    _archive(RecordType::PathRef, command, *v);

  } else if (auto v = dynamic_cast<ExpectResult*>(step)) {
    _archive(RecordType::ExpectResult, command, *v);

  } else if (auto v = dynamic_cast<MatchMetadata*>(step)) {
    _archive(RecordType::MatchMetadata, command, *v);

  } else if (auto v = dynamic_cast<MatchContent*>(step)) {
    _archive(RecordType::MatchContent, command, *v);

  } else if (auto v = dynamic_cast<UpdateMetadata*>(step)) {
    _archive(RecordType::UpdateMetadata, command, *v);

  } else if (auto v = dynamic_cast<UpdateContent*>(step)) {
    _archive(RecordType::UpdateContent, command, *v);

  } else if (auto v = dynamic_cast<Launch*>(step)) {
    _archive(RecordType::Launch, command, *v);

  } else if (auto v = dynamic_cast<Join*>(step)) {
    _archive(RecordType::Join, command, *v);

  } else if (auto v = dynamic_cast<Exit*>(step)) {
    _archive(RecordType::Exit, command, *v);

  } else {
    FAIL << "Unknown IR step type";
  }
}

void OutputTrace::addStep(shared_ptr<Command> c, unique_ptr<Step>&& s) noexcept {
  _steps.emplace_back(c, std::move(s));
}

/// Serialization function for struct timespec
template <class Archive>
void serialize(Archive& ar, struct timespec& ts) noexcept {
  ar(ts.tv_sec, ts.tv_nsec);
}

/// Serialization function for std::filesystem::path
namespace std {
  namespace filesystem {
    template <class Archive>
    void load(Archive& ar, path& p) {
      string s;
      ar(s);
      p = s;
    }

    template <class Archive>
    void save(Archive& ar, const path& p) {
      ar(string(p));
    }
  }
}

/** Register types and polymorphic relationships **/

// Versions
CEREAL_REGISTER_TYPE(MetadataVersion);
CEREAL_REGISTER_TYPE(FileVersion);
CEREAL_REGISTER_TYPE(SymlinkVersion);

// Directory version types
CEREAL_REGISTER_TYPE(AddEntry);
CEREAL_REGISTER_TYPE(RemoveEntry);
CEREAL_REGISTER_TYPE(CreatedDir);
CEREAL_REGISTER_TYPE(ListedDir);
