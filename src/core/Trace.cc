#include "Trace.hh"

#include <filesystem>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
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
  shared_ptr<Trace> result(new Trace());
  result->init();
  return result;
}

void Trace::init() noexcept {
  // Create the initial pipe references
  _stdin = make_shared<Pipe>();
  _stdout = make_shared<Pipe>();
  _stderr = make_shared<Pipe>();

  // Set up the root and working directory references
  _root = make_shared<Access>(nullptr, "/", AccessFlags{.x = true});
  _cwd = make_shared<Access>(_root, fs::current_path().relative_path(), AccessFlags{.x = true});

  // Set up the reference to the dodo-launch executable
  fs::path dodo = readlink("/proc/self/exe");
  fs::path dodo_launch = dodo.parent_path() / "dodo-launch";
  AccessFlags exe_flags;
  exe_flags.r = true;
  exe_flags.x = true;
  _exe = make_shared<Access>(_root, dodo_launch.relative_path(), exe_flags);

  // Create a map of initial file descriptors
  map<int, FileDescriptor> fds = {{0, FileDescriptor(_stdin, false)},
                                  {1, FileDescriptor(_stdout, true)},
                                  {2, FileDescriptor(_stderr, true)}};

  // Make a root command
  auto root_cmd = make_shared<Command>(_exe, vector<string>{"dodo-launch"}, fds, _cwd, _root);

  // Make a launch action for the root command
  auto launch = make_shared<Launch>(root_cmd);

  // Add the launch to the trace
  _steps.emplace_back(nullptr, launch);
}

void Trace::resolveReferences(Env& env) noexcept {
  // Resolve stdin
  _stdin->resolvesTo(env.getPipe(nullptr));
  _stdin->getArtifact()->setName("stdin");
  auto stdin_pipe = _stdin->getArtifact()->as<PipeArtifact>();
  stdin_pipe->setFDs(0, -1);

  // Resolve stdout
  _stdout->resolvesTo(env.getPipe(nullptr));
  _stdout->getArtifact()->setName("stdout");
  auto stdout_pipe = _stdout->getArtifact()->as<PipeArtifact>();
  stdout_pipe->setFDs(-1, 1);

  // Resolve stderr
  _stderr->resolvesTo(env.getPipe(nullptr));
  _stderr->getArtifact()->setName("stderr");
  auto stderr_pipe = _stderr->getArtifact()->as<PipeArtifact>();
  stderr_pipe->setFDs(-1, 2);

  // Resolve the root directory
  _root->resolvesTo(env.getRootDir());

  // Resolve the current working directory
  _cwd->resolvesTo(env.getRootDir()->resolve(nullptr, "/", _cwd->getRelativePath(), _cwd, true));
  _cwd->getArtifact()->setName(".");

  // Resolve the dodo-launch executable
  _exe->resolvesTo(env.getRootDir()->resolve(nullptr, "/", _exe->getRelativePath(), _exe, true));
}

// Print this trace
ostream& Trace::print(ostream& o) const noexcept {
  // Print the pre-build references
  o << _stdin << endl;
  o << _stdout << endl;
  o << _stderr << endl;
  o << _root << endl;
  o << _cwd << endl;
  o << _exe << endl;

  for (auto& [c, s] : _steps) {
    if (c) {
      o << c << ": " << s << endl;
    } else {
      o << s << endl;
    }
  }
  return o;
}
