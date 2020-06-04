#include "Build.hh"

#include <iostream>
#include <memory>
#include <ostream>

#include "data/IR.hh"
#include "tracing/Tracer.hh"
#include "ui/options.hh"

using std::cout;
using std::endl;
using std::ostream;
using std::shared_ptr;

void Build::run() {
  // This is a hack to resolve the stdin, stdout, and stderr pipes before starting emulation.
  for (auto& [index, info] : _root->getInitialFDs()) {
    info.getReference()->emulate(_root, *this);
  }

  // Inform observers of the launch action
  for (auto& o : _observers) {
    o->launchRootCommand(_root);
  }

  // Start the build by running the root command
  runCommand(_root);

  // Ask the environment to check remaining artifacts for changes, and to save metadata and
  // fingerprints for artifacts that were created during the build
  _env.finalize();
}

// Called when an emulated command launches another command
void Build::launch(shared_ptr<Command> parent, shared_ptr<Command> child) {
  // Inform observers of the launch action
  for (auto& o : _observers) {
    o->launchChildCommand(parent, child);
  }

  // Now actually run the command
  runCommand(child);
}

void Build::runCommand(shared_ptr<Command> c) {
  if (checkRerun(c)) {
    // We are rerunning this command, so clear the lists of steps and children
    c->reset();

    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run) cout << c->getFullName() << endl;

    // Actually run the command, unless this is a dry run
    if (!options::dry_run) {
      // Set up a tracing context and run the command
      Tracer(*this).run(c);
    }

  } else {
    c->emulate(*this);
  }
}

ostream& Build::print(ostream& o) const {
  if (_rerun.size() > 0) {
    o << "The following commands will be rerun:" << endl;
    for (auto& c : _rerun) {
      o << "  " << c << endl;
    }

  } else {
    o << "No commands to rerun" << endl;
  }

  return o;
}
