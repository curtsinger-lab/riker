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

InputTrace::InputTrace(string filename) noexcept {
  try {
    // Open the file for reading. Must pass std::ios::binary!
    ifstream f(filename, std::ios::binary);

    // Initialize cereal's binary archive reader
    cereal::BinaryInputArchive archive(f);

    // Loop until we hit the end of the trace
    bool at_end = false;
    while (!at_end) {
      // Load a command and step from the trace archive
      shared_ptr<Command> command;
      unique_ptr<Step> step;
      archive(command, step);

      // If the command and step are both nullptr, that indicates the end of the trace
      if (command == nullptr && step == nullptr) {
        at_end = true;
      } else {
        // If we receieved a non-null step, add it to the trace list
        _steps.emplace_back(command, std::move(step));
      }
    }
  } catch (cereal::Exception& e) {
    // If loading failed, initialize a default trace
    initDefault();
  }
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

OutputTrace::OutputTrace(string filename) noexcept :
    _output(filename, std::ios::binary), _archive(_output) {}

OutputTrace::~OutputTrace() noexcept {
  // Write out the list of steps
  for (auto& [c, s] : _steps) {
    _archive(c, s);
  }

  // Write the null marker at the end of the trace
  shared_ptr<Command> c = nullptr;
  unique_ptr<Step> s = nullptr;
  _archive(c, s);
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

enum Foo { Bar = 1, Baz = 2, Flib = 2 };

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

// IR Steps
CEREAL_REGISTER_TYPE(SpecialRef);
CEREAL_REGISTER_TYPE(PipeRef);
CEREAL_REGISTER_TYPE(FileRef);
CEREAL_REGISTER_TYPE(SymlinkRef);
CEREAL_REGISTER_TYPE(DirRef);
CEREAL_REGISTER_TYPE(PathRef);
CEREAL_REGISTER_TYPE(ExpectResult);
CEREAL_REGISTER_TYPE(MatchMetadata);
CEREAL_REGISTER_TYPE(MatchContent);
CEREAL_REGISTER_TYPE(UpdateMetadata);
CEREAL_REGISTER_TYPE(UpdateContent);
CEREAL_REGISTER_TYPE(Launch);
CEREAL_REGISTER_TYPE(Join);
CEREAL_REGISTER_TYPE(Exit);
