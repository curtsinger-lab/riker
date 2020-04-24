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
#include "tracing/Tracer.hh"
#include "ui/log.hh"

using std::cout;
using std::endl;
using std::list;
using std::make_shared;
using std::map;
using std::ofstream;
using std::string;

void Build::run(Tracer& tracer) {
  FAIL_IF(!_root) << "Cannot run an empty build";

  // Set up an environment that will be used to emulate filesystem operations
  Env env;

  // This set will track all commands that have to be run because they have changed inputs, or
  // produce final outputs that are removed/overwritten
  set<shared_ptr<Command>> changed;

  // Run through the command trace to find commands whose inputs have changed
  _root->checkInputs(env, changed);

  // Check the final state of the environment against the actual filesystem
  env.checkFinalState(changed);

  // We'll now build a set of commands that have to rerun to update the build
  set<shared_ptr<Command>> to_rerun;

  // Mark each command in the command set, producing a (potentially larger) set of commands that
  // must be rerun directly
  for (auto& c : changed) {
    c->mark(to_rerun);
  }

  // Finally, build a minimal set of commands that includes only marked commands that are not
  // descendants of marked commands. This is the set of commands we will actually invoke.
  set<shared_ptr<Command>> rerun_ancestors;
  _root->getMarkedAncestors(rerun_ancestors);

  // Run the root command with the tracer
  _root->run(rerun_ancestors, tracer);

  // Finish up tracing by finalizing all artifacts
  tracer.finalize();
}

void Build::check() {
  FAIL_IF(!_root) << "Cannot run build check on an empty build";

  // Set up an environment that will be used to emulate filesystem operations
  Env env;

  // This set will track all commands with inputs that are changed directly
  set<shared_ptr<Command>> changed;

  // Run through the command trace to populate the environment and changed set
  _root->checkInputs(env, changed);

  // Print the commands whose inputs have changed
  if (changed.size() > 0) {
    // Print commands that have changed inputs
    cout << "Commands with changed inputs:" << endl;
    for (auto& c : changed) {
      cout << "  " << c << endl;
    }
    cout << endl;
  }

  // This set will track commands whose output has been changed or removed
  set<shared_ptr<Command>> needed;

  // Check the final state of the environment against the actual filesystem
  env.checkFinalState(needed);

  // Print the commands whose outputs have been replaced
  if (needed.size() > 0) {
    // Print commands with needed outputs
    cout << "Commands whose output is needed:" << endl;
    for (auto& c : needed) {
      cout << "  " << c << endl;
    }
    cout << endl;
  }

  // We'll now build a set of commands that have to rerun to update the build
  set<shared_ptr<Command>> to_rerun;

  // Mark each command with changed inputs to populate the to_rerun set
  for (auto& c : changed) {
    c->mark(to_rerun);
  }

  // Mark each command with missing outputs as well
  for (auto& c : needed) {
    c->mark(to_rerun);
  }

  // Finally, build a minimal set of commands that includes only marked commands that are not
  // descendants of marked commands. This is the set of commands we will actually invoke.
  set<shared_ptr<Command>> rerun_ancestors;
  _root->getMarkedAncestors(rerun_ancestors);

  if (to_rerun.size() > 0) {
    cout << "A rebuild must run the following commands:" << endl;
    for (auto& c : to_rerun) {
      cout << "  " << c;
      if (rerun_ancestors.find(c) != rerun_ancestors.end()) {
        cout << " *";
      }
      cout << endl;
    }
    cout << "Only commands marked with * will be executed." << endl;
    cout << "Others are descendants of marked commands and will be run implicitly." << endl;
  } else {
    cout << "No changes detected" << endl;
  }
}
