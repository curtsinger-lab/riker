#include "Build.hh"

#include <memory>

#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "core/Rebuild.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"

using std::make_shared;

Build::Build() {
  _std_pipes[0] = make_shared<Artifact>("stdin");
  _std_pipes[1] = make_shared<Artifact>("stdout");
  _std_pipes[2] = make_shared<Artifact>("stderr");

  _std_refs[0] = make_shared<Pipe>();
  _std_refs[1] = make_shared<Pipe>();
  _std_refs[2] = make_shared<Pipe>();

  map<int, FileDescriptor> default_fds = {{0, FileDescriptor(_std_refs[0], _std_pipes[0], false)},
                                          {1, FileDescriptor(_std_refs[1], _std_pipes[1], true)},
                                          {2, FileDescriptor(_std_refs[2], _std_pipes[2], true)}};

  _root = Command::createRootCommand(default_fds);
}

void Build::run(Rebuild& rebuild, Tracer& tracer) {
  FAIL_IF(!_root) << "Cannot run an empty build";

  // Run the root command with the tracer
  _root->run(rebuild, tracer);

  // Finish up tracing by finalizing all artifacts
  tracer.finalize();
}
