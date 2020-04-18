#include "Build.hh"

#include <fstream>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <string>

#include "core/Artifact.hh"
#include "core/Command.hh"
#include "core/Env.hh"
#include "core/FileDescriptor.hh"
#include "core/IR.hh"
#include "ui/log.hh"

using std::cout;
using std::endl;
using std::list;
using std::make_shared;
using std::map;
using std::ofstream;
using std::string;

Build::Build(string executable, vector<string> arguments) {
  // Create references for the three default pipes: stdin, stdout, and stderr
  _default_refs[0] = make_shared<Pipe>();
  _default_refs[1] = make_shared<Pipe>();
  _default_refs[2] = make_shared<Pipe>();

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

void Build::check() {
  FAIL_IF(!_root) << "Cannot run build check on an empty build";

  // Set up an environment that will be used to emulate filesystem operations
  Env env;

  // This set will track all commands with inputs that are changed directly
  set<shared_ptr<Command>> changed;

  // Run through the command trace to populate the environment and changed set
  _root->checkInputs(env, changed);

  // We'll now build a set of commands that have to rerun to update the build
  set<shared_ptr<Command>> to_rerun;

  // Mark each command with changed inputs to populate the to_rerun set
  for (auto& c : changed) {
    c->mark(to_rerun);
  }

  // Print some information
  if (changed.size() > 0) {
    // Print commands that have changed inputs
    cout << "Commands with changed inputs:" << endl;
    for (auto& c : changed) {
      cout << "  " << c << endl;
    }
    cout << endl;

    cout << "A rebuild must run the following commands:" << endl;
    for (auto& c : to_rerun) {
      cout << "  " << c << endl;
    }

  } else {
    cout << "No changes detected" << endl;
  }
}
