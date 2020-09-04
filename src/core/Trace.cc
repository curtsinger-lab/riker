#include "Trace.hh"

#include <filesystem>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "build/Env.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"

using std::endl;
using std::make_shared;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;

namespace fs = std::filesystem;

shared_ptr<Trace> Trace::getDefault() noexcept {
  auto trace = make_shared<Trace>();

  // Create the initial pipe references
  auto stdin_ref = make_shared<RefResult>();
  trace->_steps.emplace_back(nullptr, make_shared<SpecialRef>(SpecialRef::stdin, stdin_ref));

  auto stdout_ref = make_shared<RefResult>();
  trace->_steps.emplace_back(nullptr, make_shared<SpecialRef>(SpecialRef::stdout, stdout_ref));

  auto stderr_ref = make_shared<RefResult>();
  trace->_steps.emplace_back(nullptr, make_shared<SpecialRef>(SpecialRef::stderr, stderr_ref));

  // Create a reference to the root directory
  auto root_ref = make_shared<RefResult>();
  trace->_steps.emplace_back(nullptr, make_shared<SpecialRef>(SpecialRef::root, root_ref));

  // Create a reference to the current working directory and add it to the trace
  auto cwd_ref = make_shared<RefResult>();
  trace->_steps.emplace_back(nullptr, make_shared<SpecialRef>(SpecialRef::cwd, cwd_ref));

  // Set up the reference to the dodo-launch executable and add it to the trace
  auto exe_ref = make_shared<RefResult>();
  trace->_steps.emplace_back(nullptr, make_shared<SpecialRef>(SpecialRef::launch_exe, exe_ref));

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(stdin_ref, AccessFlags{.r = true})},
                                  {1, FileDescriptor(stdout_ref, AccessFlags{.w = true})},
                                  {2, FileDescriptor(stderr_ref, AccessFlags{.w = true})}};

  // Make a root command
  auto root_cmd =
      make_shared<Command>(exe_ref, vector<string>{"dodo-launch"}, fds, cwd_ref, root_ref);

  // Make a launch action for the root command
  trace->_steps.emplace_back(nullptr, make_shared<Launch>(root_cmd));

  return trace;
}

// Print this trace
ostream& Trace::print(ostream& o) const noexcept {
  for (auto& [c, s] : _steps) {
    if (c) {
      o << c << ": " << s << endl;
    } else {
      o << s << endl;
    }
  }
  return o;
}
