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
  for (auto& [_, entry] : _artifacts) {
    auto& [path, artifact] = entry;
    auto ref = make_shared<Access>(path, AccessFlags());
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
  if (auto p = dynamic_pointer_cast<Pipe>(ref)) {
    // Look to see if we've already resolve this reference to an artifact
    auto iter = _pipes.find(p);
    if (iter == _pipes.end()) {
      // Create the artifact for this pipe
      auto artifact = make_shared<Artifact>(ref);

      // Creating a pipe sets the pipe's contents as well
      c->setContents(ref, artifact);

      // Add the record for this pipe
      iter = _pipes.emplace_hint(iter, p, artifact);
    }

    // Return the artifact, which was either found or inserted
    return iter->second;

  } else if (auto a = dynamic_pointer_cast<Access>(ref)) {
    string p = a->getPath();
    bool follow_links = !a->getFlags().nofollow;

    // Now that we have a path, we can stat it
    struct stat statbuf;
    int rc;
    if (follow_links) {
      rc = stat(p.c_str(), &statbuf);
    } else {
      rc = lstat(p.c_str(), &statbuf);
    }

    // If the stat call failed, return an empty artifact
    if (rc) return shared_ptr<Artifact>();

    // The stat call succeeded. Check for an existing inode entry
    auto iter = _artifacts.find(statbuf.st_ino);
    if (iter == _artifacts.end()) {
      // Create an artifact
      auto artifact = make_shared<Artifact>(ref);

      // Did the reference create this artifact?
      if (created) {
        // Yes. Generate the new version and add the IR step to the command that creates it
        auto v = artifact->setContents(c);
        c->addStep(make_shared<SetContents>(ref, v));

      } else {
        // No. This version is just opened
        artifact->createInitialVersion();
      }

      // Add the artifact to the map
      iter = _artifacts.emplace_hint(iter, statbuf.st_ino,
                                     pair<string, shared_ptr<Artifact>>{p, artifact});
    }

    // Return the found/inserted artifact
    return iter->second.second;
  } else {
    FAIL << "Unsupported reference type";
    abort();
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
    // Handle each IR type here
    if (auto pipe = dynamic_pointer_cast<Pipe>(step)) {
      // Nothing to do for pipes

    } else if (auto access = dynamic_pointer_cast<Access>(step)) {
      // Nothing to do for access

    } else if (auto ref_result = dynamic_pointer_cast<ReferenceResult>(step)) {
      // Check if the reference resolves the same way
      if (!checkAccess(c, ref_result->getReference(), ref_result->getResult())) {
        LOG << c << " changed: " << ref_result;
        changed = true;
      }

    } else if (auto m_match = dynamic_pointer_cast<MetadataMatch>(step)) {
      // Check if the metadata in the environment matches the expected version
      if (!checkMetadata(c, m_match->getReference(), m_match->getVersion())) {
        LOG << c << " changed: " << m_match;
        changed = true;
      }

    } else if (auto c_match = dynamic_pointer_cast<ContentsMatch>(step)) {
      // Check if the contents in the environment match the expected version
      if (!checkContents(c, c_match->getReference(), c_match->getVersion())) {
        LOG << c << " changed: " << c_match;
        changed = true;
      }

    } else if (auto launch = dynamic_pointer_cast<Launch>(step)) {
      // Check the child command's inputs
      findChanges(launch->getCommand());

    } else if (auto set_metadata = dynamic_pointer_cast<SetMetadata>(step)) {
      // Update the metadata in the environment
      setMetadata(c, set_metadata->getReference(), set_metadata->getVersion());

    } else if (auto set_contents = dynamic_pointer_cast<SetContents>(step)) {
      // Update the contents in the environment
      setContents(c, set_contents->getReference(), set_contents->getVersion());

    } else {
      FAIL << "Unsupported IR action " << step;
    }
  }

  // If anything was different, add c to the set of commands with changed inputs
  if (changed) _changed.insert(c);
}

void Rebuild::checkFinalState() {
  // Loop over all entries in the environment
  for (auto& [ref, entry] : _env.getArtifacts()) {
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

// Check if an access resolves as-expected in the current environment
bool Rebuild::checkAccess(shared_ptr<Command> c, shared_ptr<Reference> ref, int expected) {
  auto [artifact, rc, created] = _env.get(c, ref);

  if (rc == SUCCESS) {
    recordDependency(c, artifact);
  }

  return rc == expected;
}

bool Rebuild::checkMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref,
                            shared_ptr<Version> v) {
  auto [a, rc, created] = _env.get(c, ref);
  FAIL_IF(rc != SUCCESS) << "Failed to get artifact for metadata check";
  FAIL_IF(!a->getLatestVersion()) << "Artifact for " << ref << " has no versions";

  recordDependency(c, a);

  return v->metadataMatch(a->getLatestVersion());
}

bool Rebuild::checkContents(shared_ptr<Command> c, shared_ptr<Reference> ref,
                            shared_ptr<Version> v) {
  auto [a, rc, created] = _env.get(c, ref);
  FAIL_IF(rc != SUCCESS) << "Failed to get artifact for contents check";

  recordDependency(c, a);

  return v->fingerprintMatch(a->getLatestVersion());
}

void Rebuild::setMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref, shared_ptr<Version> v) {
  auto [a, rc, created] = _env.get(c, ref);
  FAIL_IF(rc != SUCCESS) << "Failed to get artifact";

  a->appendVersion(v, c);
}

void Rebuild::setContents(shared_ptr<Command> c, shared_ptr<Reference> ref, shared_ptr<Version> v) {
  auto [a, rc, created] = _env.get(c, ref);
  FAIL_IF(rc != SUCCESS) << "Failed to get artifact";

  a->appendVersion(v, c);
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
