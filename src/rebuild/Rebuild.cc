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
Rebuild Rebuild::create(shared_ptr<Command> root) {
  // Initialize the rebuild with the build's root command
  Rebuild r(root);

  // Identify commands with changed dependencies
  r.findChanges(root);

  // Check the final outputs of the emulated build against the filesystem
  r.checkFinalState();

  // Mark all the commands with changed inputs
  for (auto& c : r._changed) {
    c->mark();
  }

  // Mark all the commands whose output is required
  for (auto& c : r._output_needed) {
    c->mark();
  }

  return r;
}

// Run a rebuild, updating the in-memory build representation
void Rebuild::run() {
  // Create a tracing context to run the build
  Tracer tracer(*this);

  // Run or emulate the root command with the tracer
  runCommand(_root, tracer);

  // Finish up by saving metadata for any remaining artifacts
  for (auto& [ref, artifact] : _run_env.getArtifacts()) {
    artifact->getLatestVersion()->saveMetadata(ref);
    artifact->getLatestVersion()->saveFingerprint(ref);
  }
}

// Run or emulate a command in this rebuild
void Rebuild::runCommand(shared_ptr<Command> c, Tracer& tracer) {
  // Does the rebuild plan say command c must run?
  if (c->mustRerun()) {
    // We are rerunning this command, so clear the lists of steps and children
    c->reset();

    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run) cout << c->getFullName() << endl;

    // Actually run the command, unless this is a dry run
    if (!options::dry_run) tracer.run(c);

  } else {
    // Emulate this command by running its children
    for (auto& child : c->getChildren()) {
      runCommand(child, tracer);
    }
  }
}

// Get an artifact during tracing
shared_ptr<Artifact> Rebuild::getArtifact(shared_ptr<Command> c, shared_ptr<Reference> ref,
                                          bool created) {
  auto [artifact, rc, is_new] = _run_env.get(c, ref);

  // If we didn't get an artifact, return a nullptr
  if (rc != SUCCESS) return shared_ptr<Artifact>();

  // If this artifact was created, set its contents
  if (is_new) {
    c->setContents(ref, artifact);
  }

  // Return the artifact
  return artifact;
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
    queue<shared_ptr<Command>> q;
    q.push(_root);
    while (!q.empty()) {
      auto c = q.front();
      o << "  " << c << endl;
      q.pop();
      for (auto& child : c->getChildren()) {
        q.push(child);
      }
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
    if (!step->check(c, _check_env, *this)) {
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
  // Loop over all entries in the environment
  for (auto& [ref, entry] : _check_env.getArtifacts()) {
    // Check the filesystem to see if the real file matches our expected version
    if (!checkFilesystemContents(ref, entry->getLatestVersion())) {
      auto creator = entry->getCreator();
      if (creator) _output_needed.insert(creator);
    }
  }
}

void Rebuild::recordDependency(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  // Record dependency information
  auto creator = a->getCreator();
  if (creator) {
    // If creator has to rerun, c may see changed input through this artifact
    creator->outputUsedBy(c);

    // The dependency back edge depends on caching
    if (options::enable_cache && a->getLatestVersion()->isSaved()) {
      // If this artifact is cached, we could restore it before c runs.
    } else {
      // Otherwise, if c has to run then we also need to run creator to produce this input
      c->needsOutputFrom(creator);
    }
  }
}

// Check if the metadata for a file on the actual filesystem matches a saved version
bool Rebuild::checkFilesystemMetadata(shared_ptr<Access> ref, shared_ptr<Version> v) {
  auto ondisk = make_shared<Version>();
  ondisk->saveMetadata(ref);
  return v->metadataMatch(ondisk);
}

// Check if the contents of a file on the actual fileystem match a saved version
bool Rebuild::checkFilesystemContents(shared_ptr<Access> ref, shared_ptr<Version> v) {
  auto ondisk = make_shared<Version>();
  ondisk->saveFingerprint(ref);
  return v->fingerprintMatch(ondisk);
}
