#include "Build.hh"

#include <iostream>
#include <memory>
#include <ostream>

#include "artifacts/Artifact.hh"
#include "artifacts/Pipe.hh"
#include "core/IR.hh"
#include "tracing/Tracer.hh"
#include "ui/options.hh"

using std::cout;
using std::dynamic_pointer_cast;
using std::endl;
using std::ostream;
using std::shared_ptr;

void Build::run() noexcept {
  // Resolve all the initial references in the trace (root, cwd, stdin, stdout, etc.)
  _trace->resolveReferences(_env);

  // Empty the trace's list of steps, saving the old list for emulation
  auto steps = _trace->reset();

  // Walk through the steps in the trace
  for (auto& [cmd, step] : steps) {
    // Is this step from a command we are re-executing?
    if (checkRerun(cmd)) {
      // Do nothing!
    } else {
      // No. Emulate the step and re-add it to the trace
      step->emulate(cmd, *this);
      _trace->addStep(cmd, step);
    }
  }

  // Wait for all remaining processes to exit
  _tracer.wait();

  // Ask the environment to check remaining artifacts for changes, and to save metadata and
  // fingerprints for artifacts that were created during the build
  _env.finalize();
}

// Called when an emulated command launches another command
void Build::launch(shared_ptr<Command> parent, shared_ptr<Command> child) noexcept {
  // If this child hasn't run before, let the observers know
  if (!child->hasExecuted()) {
    for (const auto& o : _observers) {
      o->commandNeverRun(child);
    }
  }

  // Inform observers of the launch action
  for (const auto& o : _observers) {
    o->launch(parent, child);
  }

  // If the child command must be rerun, start it in the tracer now
  if (checkRerun(child)) {
    // We are rerunning this command, so clear its list of children
    child->reset();

    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run) cout << child->getShortName(80) << endl;

    // Actually run the command, unless this is a dry run
    if (!options::dry_run) {
      // Start the command in the tracer
      _running[child] = _tracer.start(child);
      child->setExecuted();
    }
  }
}

void Build::join(shared_ptr<Command> child) noexcept {
  // If the command is in the rerun set, tell the tracer to wait for it
  if (checkRerun(child)) {
    INFO << "Waiting for process running " << child;
    _tracer.wait(_running[child]);
  }
}

ostream& Build::print(ostream& o) const noexcept {
  if (_rerun.size() > 0) {
    o << "The following commands will be rerun:" << endl;
    for (const auto& c : _rerun) {
      o << "  " << c->getShortName(78) << endl;
    }

  } else {
    o << "No commands to rerun" << endl;
  }

  return o;
}
