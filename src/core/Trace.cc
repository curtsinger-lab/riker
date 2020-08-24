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

Trace::Trace() noexcept {
  // Create the initial pipe references
  auto stdin = make_shared<SpecialRef>(SpecialRef::stdin);
  _steps.emplace_back(nullptr, stdin);

  _stdout = make_shared<Pipe>();
  _stderr = make_shared<Pipe>();

  // Create a reference to the root directory
  _root = make_shared<Access>(nullptr, "/", AccessFlags{.x = true});

  // Create a reference to the current working directory and add it to the trace
  auto cwd = make_shared<Access>(_root, fs::current_path().relative_path(), AccessFlags{.x = true});
  _steps.emplace_back(nullptr, cwd);

  // Set up the reference to the dodo-launch executable and add it to the trace
  fs::path dodo = readlink("/proc/self/exe");
  fs::path dodo_launch = (dodo.parent_path() / "dodo-launch").relative_path();
  auto exe = make_shared<Access>(_root, dodo_launch, AccessFlags{.r = true, .x = true});
  _steps.emplace_back(nullptr, exe);

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(stdin, false)},
                                  {1, FileDescriptor(_stdout, true)},
                                  {2, FileDescriptor(_stderr, true)}};

  // Make a root command
  auto root_cmd = make_shared<Command>(exe, vector<string>{"dodo-launch"}, fds, cwd, _root);

  // Make a launch action for the root command
  auto launch = make_shared<Launch>(root_cmd);
  _steps.emplace_back(nullptr, launch);
}

void Trace::resolveRefs(Build& build, shared_ptr<Env> env) noexcept {
  // Resolve stdout
  _stdout->resolvesTo(env->getPipe(build, nullptr));
  _stdout->getArtifact()->setName("stdout");
  auto stdout_pipe = _stdout->getArtifact()->as<PipeArtifact>();
  stdout_pipe->setFDs(-1, 1);

  // Resolve stderr
  _stderr->resolvesTo(env->getPipe(build, nullptr));
  _stderr->getArtifact()->setName("stderr");
  auto stderr_pipe = _stderr->getArtifact()->as<PipeArtifact>();
  stderr_pipe->setFDs(-1, 2);

  // Resolve the root directory
  _root->resolvesTo(env->getRootDir());
}

// Print this trace
ostream& Trace::print(ostream& o) const noexcept {
  // Print the pre-build references
  o << _stdout << endl;
  o << _stderr << endl;
  o << _root << endl;

  for (auto& [c, s] : _steps) {
    if (c) {
      o << c << ": " << s << endl;
    } else {
      o << s << endl;
    }
  }
  return o;
}
