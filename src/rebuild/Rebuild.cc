#include "Rebuild.hh"

#include <cerrno>
#include <ctime>
#include <iostream>
#include <memory>
#include <ostream>
#include <queue>

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "data/Command.hh"
#include "data/IR.hh"
#include "data/Version.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::cout;
using std::dynamic_pointer_cast;
using std::endl;
using std::make_shared;
using std::ostream;
using std::queue;

// Create a rebuild plan
Rebuild::Rebuild(shared_ptr<Command> root) : _root(root) {
  // Identify commands with changed dependencies
  findChanges(_root);

  // Check the final outputs of the emulated build against the filesystem
  checkFinalState();

  // Mark all the commands with changed inputs
  for (auto& c : _changed) {
    mark(c);
  }

  // Mark all the commands whose output is required
  for (auto& c : _output_needed) {
    mark(c);
  }
}

// Run a rebuild, updating the in-memory build representation
void Rebuild::run() {
  // Reset the environment
  _env.reset();

  // Create a tracing context to run the build
  Tracer tracer(_env);

  // Run or emulate the root command with the tracer
  runCommand(_root, tracer);

  // Finish up by saving metadata for any remaining artifacts
  for (auto& [ref, artifact] : _env.getArtifacts()) {
    artifact->getLatestVersion()->saveMetadata(ref);
    artifact->getLatestVersion()->saveFingerprint(ref);
  }
}

// Run or emulate a command in this rebuild
void Rebuild::runCommand(shared_ptr<Command> c, Tracer& tracer) {
  // Does the rebuild plan say command c must run?
  if (mustRerun(c)) {
    // We are rerunning this command, so clear the lists of steps and children
    c->reset();

    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run) cout << c->getFullName() << endl;

    // Actually run the command, unless this is a dry run
    if (!options::dry_run) tracer.run(c);

  } else {
    for (auto step : c->getSteps()) {
      step->check(c, _env, *this);
      if (auto launch = dynamic_pointer_cast<Launch>(step)) {
        runCommand(launch->getCommand(), tracer);
      }
    }
  }
}

// Check if a command must rerun
bool Rebuild::mustRerun(shared_ptr<Command> c) const {
  return _rerun.find(c) != _rerun.end();
}

// Show rebuild information
ostream& Rebuild::print(ostream& o) const {
  if (_changed.size() > 0) {
    o << "Commands with changed inputs:" << endl;
    for (auto& c : _changed) {
      o << "  " << c << endl;
    }
    o << endl;
  }

  if (_output_needed.size() > 0) {
    o << "Commands whose output is missing or modified:" << endl;
    for (auto& c : _output_needed) {
      o << "  " << c << endl;
    }
    o << endl;
  }

  if (_changed.size() > 0 || _output_needed.size() > 0) {
    o << "A rebuild will run the following commands:" << endl;
    for (auto c : _rerun) {
      o << "  " << c << endl;
    }
  } else {
    o << "No changes detected" << endl;
  }

  return o;
}

void Rebuild::findChanges(shared_ptr<Command> c) {
  // Keep track of whether we've seen any changes for command c
  bool changed = false;

  // If this command has never run, it is definitely changed
  if (c->neverRun()) {
    LOG << c << " changed: never run";
    changed = true;
  }

  // Loop over the steps from the command trace to see if command c will see any changes
  for (auto step : c->getSteps()) {
    // Check whether the IR step runs as expected in the emulated environment
    if (!step->check(c, _env, *this)) {
      // If check() returns false, something has changed
      LOG << c << " changed: " << step;
      changed = true;
    }

    // If this is a launch action, check the child command
    if (auto launch = dynamic_pointer_cast<Launch>(step)) {
      findChanges(launch->getCommand());
    }
  }

  // If anything was different, add c to the set of commands with changed inputs
  if (changed) _changed.insert(c);
}

void Rebuild::checkFinalState() {
  // Loop over all the artifacts left in the environment at the end of the build
  for (auto& [ref, a] : _env.getArtifacts()) {
    // If this artifact was not created by any command, we can't do anything about it
    auto creator = a->getCreator();
    if (!creator) continue;

    auto latest = a->getLatestVersion();

    // If this artifact's final version is cached, we can just stage it in
    if (options::enable_cache && latest->isSaved()) continue;

    // Create a version that represents the on-disk contents reached through this reference
    auto v = make_shared<Version>();
    v->saveFingerprint(ref);

    // If the fingerprint doesn't match we will need to rerun the creator
    if (!v->fingerprintMatch(a->getLatestVersion())) {
      _output_needed.insert(creator);
    }
  }
}

// Command c depends on artifact a
void Rebuild::addDependency(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  auto creator = a->getCreator();
  if (creator) {
    // Output from creator is used by c. If creator reruns, c may have to rerun.
    _output_used_by[creator].insert(c);

    // The dependency back edge depends on caching
    if (options::enable_cache && a->getLatestVersion()->isSaved()) {
      // If this artifact is cached, we could restore it before c runs.
    } else {
      // Otherwise, if c has to run then we also need to run creator to produce this input
      _needs_output_from[c].insert(creator);
    }
  }
}

// Mark command c for rerun, and propagate that marking to other required commands
void Rebuild::mark(shared_ptr<Command> c) {
  // If this command is already marked, there's no work to do
  if (_rerun.find(c) != _rerun.end()) return;

  // Mark this command
  _rerun.insert(c);

  // Mark this command's children
  for (auto& child : c->getChildren()) {
    mark(child);
  }

  // Mark any commands that produce output that this command needs
  for (auto& other : _needs_output_from[c]) {
    mark(other);
  }

  // Mark any commands that use this command's output
  for (auto& other : _output_used_by[c]) {
    mark(other);
  }
}
