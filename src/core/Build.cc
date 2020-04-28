#include "Build.hh"

#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <string>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"

using std::cout;
using std::endl;
using std::list;
using std::make_shared;
using std::map;
using std::ofstream;
using std::string;

void Build::run(Rebuild& rebuild, Tracer& tracer) {
  FAIL_IF(!_root) << "Cannot run an empty build";

  // Run the root command with the tracer
  _root->run(rebuild, tracer);

  // Finish up tracing by finalizing all artifacts
  tracer.finalize();
}
