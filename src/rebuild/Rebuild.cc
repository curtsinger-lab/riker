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
#include "rebuild/Build.hh"
#include "tracing/Tracer.hh"
#include "ui/log.hh"
#include "ui/options.hh"

using std::cout;
using std::endl;
using std::make_shared;
using std::ostream;

void Rebuild::planBuild(Build& b) const {
  // Mark all the commands with changed inputs
  for (auto& c : _changed) {
    mark(b, c);
  }

  // Mark all the commands whose output is required
  for (auto& c : _output_needed) {
    mark(b, c);
  }
}

// Mark command c for rerun, and propagate that marking to other required commands
void Rebuild::mark(Build& b, shared_ptr<Command> c) const {
  // Mark command c for rerun. If the command was already marked, setRerun will return false and
  // we can stop here
  if (!b.setRerun(c)) return;

  // Mark this command's children
  for (auto& child : c->getChildren()) {
    mark(b, child);
  }

  // Mark any commands that produce output that this command needs
  if (auto iter = _needs_output_from.find(c); iter != _needs_output_from.end()) {
    for (auto& other : iter->second) {
      mark(b, other);
    }
  }

  // Mark any commands that use this command's output
  if (auto iter = _output_used_by.find(c); iter != _output_used_by.end()) {
    for (auto& other : iter->second) {
      mark(b, other);
    }
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

  return o;
}

// Command c depends on the metadata for artifact a
void Rebuild::metadataInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  // TODO: any check for caching/staging here?
  // It seems like we can always stage in metadata changes, since we store all of the relevant stat
  // fields and could put them in place for any command.
}

// Command c depends on the contents of artifact a
void Rebuild::contentInput(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  // During the planning phase, record this dependency
  auto creator = a->getCreator();
  if (creator) {
    // Output from creator is used by c. If creator reruns, c may have to rerun.
    _output_used_by[creator].insert(c);

    // The dependency back edge depends on caching
    if (options::enable_cache && a->isSaved()) {
      // If this artifact is cached, we could restore it before c runs.
    } else {
      // Otherwise, if c has to run then we also need to run creator to produce this input
      _needs_output_from[c].insert(creator);
    }
  }
}

void Rebuild::metadataMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  // Record the change
  LOG << c << " observed metadata change in " << a;
  _changed.insert(c);
}

void Rebuild::contentMismatch(shared_ptr<Command> c, shared_ptr<Artifact> a) {
  // Record the change
  LOG << c << " observed content change in " << a;
  _changed.insert(c);
}

// IR step s in command c observed a change. This method is called by emulate() in Step
void Rebuild::commandNeverRun(shared_ptr<Command> c) {
  // Record the change
  LOG << c << " never run";
  _changed.insert(c);
}

// IR step s in command c observed a change. This method is called by emulate() in Step
void Rebuild::commandChanged(shared_ptr<Command> c, shared_ptr<const Step> s) {
  // Record the change
  LOG << c << " changed: " << s;
  _changed.insert(c);
}

// An artifact's final contents do not match what is on the filesystem
void Rebuild::finalContentMismatch(shared_ptr<Artifact> a) {
  // If this artifact was not created by any command, there's nothing we can do about it
  if (!a->getCreator()) return;

  // If this artifact's final version is cached, we can just stage it in
  if (options::enable_cache && a->isSaved()) return;

  // Otherwise we have to run the command that created this artifact
  _output_needed.insert(a->getCreator());
}
