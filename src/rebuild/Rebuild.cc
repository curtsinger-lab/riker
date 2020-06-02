#include "Rebuild.hh"

#include <iostream>
#include <memory>
#include <ostream>

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
using std::endl;
using std::make_shared;
using std::ostream;

// Create a rebuild plan
Rebuild::Rebuild(shared_ptr<Command> root) : _root(root), _env(*this) {
  // Record that we are in the planning phase of the rebuild
  _phase = RebuildPhase::Planning;

  // If the root command has never run, mark it as changed
  if (_root->neverRun()) {
    LOG << _root << " changed: never run";
    _changed.insert(_root);
  } else {
    // Otherwise, emulate the root command to track dependencies and changes
    _root->emulate(_env);
  }

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
  // The rebuild is now in the running phase
  _phase = RebuildPhase::Running;

  // Reset the environment
  _env.reset();

  // Run or emulate the root command with the tracer
  runCommand(_root);

  // Finish up by saving metadata and fingerprints for any artifacts left after the build
  for (auto& [ref, artifact] : _env.getArtifacts()) {
    artifact->saveMetadata(ref);
    artifact->saveFingerprint(ref);
  }
}

// Run or emulate a command in this rebuild
void Rebuild::runCommand(shared_ptr<Command> c) {
  // Does the rebuild plan say command c must run?
  if (_rerun.find(c) != _rerun.end()) {
    // We are rerunning this command, so clear the lists of steps and children
    c->reset();

    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run) cout << c->getFullName() << endl;

    // Actually run the command, unless this is a dry run
    if (!options::dry_run) {
      // Set up a tracing context and run the command
      Tracer(_env).run(c);
    }

  } else {
    c->emulate(_env);
  }
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

// Command c depends on the metadata for artifact a
void Rebuild::addMetadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  // TODO: any check for caching/staging here?
  // It seems like we can always stage in metadata changes, since we store all of the relevant stat
  // fields and could put them in place for any command.
}

// Command c depends on the contents of artifact a
void Rebuild::addContentInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  // Are we planning or running the rebuild?
  if (_phase == RebuildPhase::Planning) {
    // During the planning phase, record this dependency
    auto creator = a->getCreator();
    if (creator) {
      // Output from creator is used by c. If creator reruns, c may have to rerun.
      _output_used_by[creator].insert(c);

      // The dependency back edge depends on caching
      if (options::enable_cache && a->getContents()->isSaved()) {
        // If this artifact is cached, we could restore it before c runs.
      } else {
        // Otherwise, if c has to run then we also need to run creator to produce this input
        _needs_output_from[c].insert(creator);
      }
    }
  } else {
    // We don't need to record new dependencies during the rebuild. At the moment, addInput is only
    // called on emulated commands. If we also call it on traced commands we could use this point to
    // detect when commands gain a new dependency on rerun. That could be especially useful for
    // synchronizing parallel rebuilds.
  }
}

void Rebuild::mismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  // Are we planning or running the rebuild?
  if (_phase == RebuildPhase::Planning) {
    // Record the change
    LOG << c << " changed: " << a;
    _changed.insert(c);
  } else {
    // We shouldn't detect changes in the running phase, so this is a failure
    WARN << "Unexpected change detected during rebuild: " << c;
  }
}

// IR step s in command c observed a change. This method is called by emulate() in Step
void Rebuild::changed(shared_ptr<Command> c, shared_ptr<const Step> s) {
  // Are we planning or running the rebuild?
  if (_phase == RebuildPhase::Planning) {
    // Record the change
    LOG << c << " changed: " << s;
    _changed.insert(c);
  } else {
    // We shouldn't detect changes in the running phase, so this is a failure
    WARN << "Unexpected change detected during rebuild: " << c << ", " << s;
  }
}

// A parent command is launching a child command
void Rebuild::launched(shared_ptr<Command> parent, shared_ptr<Command> child) {
  // Are we planning or running the rebuild?
  if (_phase == RebuildPhase::Planning) {
    // In the planning phase, we always emulate the child command to capture its dependencies
    child->emulate(_env);
  } else {
    // If we are actually running the rebuild, call runCommand to decide whether to run or emulate
    runCommand(child);
  }
}

// Check to see if artifacts left at the end of an emulated build match the on-disk state
void Rebuild::checkFinalState() {
  // Loop over all the artifacts left in the environment at the end of the build
  for (auto& [ref, a] : _env.getArtifacts()) {
    // If this artifact was not created by any command, we can't do anything about it
    auto creator = a->getCreator();
    if (!creator) continue;

    // If this artifact's final version is cached, we can just stage it in
    if (options::enable_cache && a->getContents()->isSaved()) continue;

    // Create a version that represents the on-disk contents reached through this reference
    auto v = make_shared<Version>();
    v->saveFingerprint(ref);

    // If the fingerprint doesn't match we will need to rerun the creator
    if (!v->contentsMatch(a->getContents())) {
      _output_needed.insert(creator);
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
