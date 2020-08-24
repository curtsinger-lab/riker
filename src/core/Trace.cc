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
#include "util/path.hh"

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
  auto stdin = make_shared<SpecialRef>(SpecialRef::stdin);
  trace->_steps.emplace_back(nullptr, stdin);

  auto stdout = make_shared<SpecialRef>(SpecialRef::stdout);
  trace->_steps.emplace_back(nullptr, stdout);

  auto stderr = make_shared<SpecialRef>(SpecialRef::stderr);
  trace->_steps.emplace_back(nullptr, stderr);

  // Create a reference to the root directory
  auto root = make_shared<SpecialRef>(SpecialRef::root);
  trace->_steps.emplace_back(nullptr, root);

  // Create a reference to the current working directory and add it to the trace
  auto cwd = make_shared<Access>(root, fs::current_path().relative_path(), AccessFlags{.x = true});
  trace->_steps.emplace_back(nullptr, cwd);

  // Set up the reference to the dodo-launch executable and add it to the trace
  fs::path dodo = readlink("/proc/self/exe");
  fs::path dodo_launch = (dodo.parent_path() / "dodo-launch").relative_path();
  auto exe = make_shared<Access>(root, dodo_launch, AccessFlags{.r = true, .x = true});
  trace->_steps.emplace_back(nullptr, exe);

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(stdin, false)},
                                  {1, FileDescriptor(stdout, true)},
                                  {2, FileDescriptor(stderr, true)}};

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
