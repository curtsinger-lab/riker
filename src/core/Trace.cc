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
  auto stdin_ref = make_shared<SpecialRef>(SpecialRef::stdin);
  auto stdin = make_shared<Resolve>(stdin_ref);
  trace->_steps.emplace_back(nullptr, stdin);

  auto stdout_ref = make_shared<SpecialRef>(SpecialRef::stdout);
  auto stdout = make_shared<Resolve>(stdout_ref);
  trace->_steps.emplace_back(nullptr, stdout);

  auto stderr_ref = make_shared<SpecialRef>(SpecialRef::stderr);
  auto stderr = make_shared<Resolve>(stderr_ref);
  trace->_steps.emplace_back(nullptr, stderr);

  // Create a reference to the root directory
  auto root_ref = make_shared<SpecialRef>(SpecialRef::root);
  auto root = make_shared<Resolve>(root_ref);
  trace->_steps.emplace_back(nullptr, root);

  // Create a reference to the current working directory and add it to the trace
  auto cwd_ref = make_shared<SpecialRef>(SpecialRef::cwd);
  auto cwd = make_shared<Resolve>(cwd_ref);
  trace->_steps.emplace_back(nullptr, cwd);

  // Set up the reference to the dodo-launch executable and add it to the trace
  auto exe_ref = make_shared<SpecialRef>(SpecialRef::launch_exe);
  auto exe = make_shared<Resolve>(exe_ref);
  trace->_steps.emplace_back(nullptr, exe);

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(stdin, AccessFlags{.r = true})},
                                  {1, FileDescriptor(stdout, AccessFlags{.w = true})},
                                  {2, FileDescriptor(stderr, AccessFlags{.w = true})}};

  // Make a root command
  auto root_cmd = make_shared<Command>(exe, vector<string>{"dodo-launch"}, fds, cwd, root);

  // Make a launch action for the root command
  auto launch = make_shared<Launch>(root_cmd);
  trace->_steps.emplace_back(nullptr, launch);

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
