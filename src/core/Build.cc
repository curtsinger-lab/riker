#include "Build.hh"

#include <fstream>
#include <list>
#include <map>
#include <memory>
#include <string>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "ui/log.hh"

using std::list;
using std::make_shared;
using std::map;
using std::ofstream;
using std::string;

Build::Build(string executable, vector<string> arguments) {
  // Create references for the three default pipes: stdin, stdout, and stderr
  _default_refs[0] = make_shared<Reference::Pipe>();
  _default_refs[1] = make_shared<Reference::Pipe>();
  _default_refs[2] = make_shared<Reference::Pipe>();

  // Create three artifacts to correspond to those same pipes
  _default_artifacts[0] = make_shared<Artifact>("stdin");
  _default_artifacts[1] = make_shared<Artifact>("stdout");
  _default_artifacts[2] = make_shared<Artifact>("stderr");

  // Build the map of initial file descriptors
  map<int, FileDescriptor> fds = {
      {0, FileDescriptor(_default_refs[0], _default_artifacts[0], false)},
      {1, FileDescriptor(_default_refs[1], _default_artifacts[1], true)},
      {2, FileDescriptor(_default_refs[2], _default_artifacts[2], true)},
  };

  // Create the root command for the build
  _root = make_shared<Command>(executable, arguments, fds);

  INFO << "Build initialized with root " << _root.get();
}

void Build::run(Tracer& tracer) {
  if (_root) _root->run(tracer);
}
