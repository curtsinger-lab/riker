#include "Build.hh"

#include <iostream>
#include <memory>
#include <ostream>

#include "artifacts/Artifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "build/AccessFilter.hh"
#include "build/Resolution.hh"
#include "core/IR.hh"
#include "tracing/Tracer.hh"
#include "ui/options.hh"
#include "versions/ContentVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::cout;
using std::endl;
using std::ostream;
using std::shared_ptr;

/// The access filter used for metadata accesses
static AccessFilter _metadata_filter;

/// The access filter used for content accesses
static AccessFilter _content_filter;

// Resolve a pipe reference on behalf of command c
Resolution Build::resolve(shared_ptr<Command> c, shared_ptr<Pipe> ref) noexcept {
  return _env.getPipe(c);
}

// Resolve an access reference on behalf of command c
Resolution Build::resolve(shared_ptr<Command> c, shared_ptr<Access> ref) noexcept {
  return _env.resolveRef(c, ref);
}

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

// Command c issued a pipe reference while being traced
shared_ptr<Pipe> Build::tracePipe(shared_ptr<Command> c) noexcept {
  auto ref = make_shared<Pipe>();
  _trace->addStep(c, ref);
  return ref;
}

// Command c accessed a path while being traced
shared_ptr<Access> Build::traceAccess(shared_ptr<Command> c,
                                      fs::path path,
                                      AccessFlags flags,
                                      shared_ptr<Access> base) noexcept {
  // For now, the reference is just one level that covers all parts of the new path
  auto ref = base->get(path, flags);
  _trace->addStep(c, ref);
  return ref;
}

// Command c accesses metadata while being traced
void Build::traceMetadataMatch(shared_ptr<Command> c, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot check for a metadata match on an unresolved reference.";

  // Do we have to log this read?
  if (!_metadata_filter.readRequired(c.get(), ref)) return;

  // Inform the artifact that this command accesses its metadata
  const auto& v = ref->getArtifact()->accessMetadata(c, ref, InputType::Accessed);

  // If the last write was from this command we don't need a fingerprint to compare.
  if (v->getCreator() != c) v->fingerprint(ref);

  // Add the IR step
  _trace->addStep(c, make_shared<MetadataMatch>(ref, v));

  // Report the read
  _metadata_filter.read(c.get(), ref);
}

// Command c sets metadata while being traced
void Build::traceSetMetadata(shared_ptr<Command> c, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot set metadata for an unresolved reference.";

  // Do we have to log this write?
  if (!_metadata_filter.writeRequired(c.get(), ref)) return;

  // Inform the artifact that this command sets its metadata
  const auto& v = ref->getArtifact()->setMetadata(c, ref);

  // Create the SetMetadata step and add it to the command
  _trace->addStep(c, make_shared<SetMetadata>(ref, v));

  // Report the write
  _metadata_filter.write(c.get(), ref, v);
}

// Command c acceses file content while being traced
void Build::traceContentsMatch(shared_ptr<Command> c, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot check for a content match on an unresolved reference: "
                            << ref;

  // Do we have to log this read?
  if (!_content_filter.readRequired(c.get(), ref)) return;

  // Inform the artifact that this command accesses its contents
  const auto& v = ref->getArtifact()->accessContents(c, ref);

  if (!v) {
    WARN << "Accessing contents of " << ref << " returned a null version";
    return;
  }

  // If the last write was from this command, we don't need a fingerprint to compare.
  if (v->getCreator() != c) v->fingerprint(ref);

  // Add the IR step
  _trace->addStep(c, make_shared<ContentsMatch>(ref, v));

  // Report the read
  _content_filter.read(c.get(), ref);
}

// Command c sets file content while being traced
void Build::traceSetContents(shared_ptr<Command> c, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot set contents for an unresolved reference.";

  // Do we have to log this write?
  if (!_content_filter.writeRequired(c.get(), ref)) return;

  // Inform the artifact that this command sets its contents
  const auto& v = ref->getArtifact()->setContents(c, ref);

  ASSERT(v) << "Setting contents of " << ref << " produced a null version";

  // Create the SetContents step and add it to the command
  _trace->addStep(c, make_shared<SetContents>(ref, v));

  // Report the write
  _content_filter.write(c.get(), ref, v);
}

// Command c accesses the contents of a symlink while being traced
void Build::traceSymlinkMatch(shared_ptr<Command> c, shared_ptr<Reference> ref) noexcept {
  ASSERT(ref->isResolved()) << "Cannot check for a content match on an unresolved reference: "
                            << ref;

  // Inform the artifact that this command accesses its contents
  const auto& v = ref->getArtifact()->readlink(c, InputType::Accessed);

  if (!v) {
    WARN << "Readlink on " << ref << " returned a null version";
    return;
  }

  // Take a fingerprint for consistency, although symlinks are always fully saved
  v->fingerprint(ref);

  // Add the IR step
  _trace->addStep(c, make_shared<SymlinkMatch>(ref, v));
}

// Command c adds an entry to a directory
void Build::traceLink(shared_ptr<Command> c,
                      shared_ptr<Reference> ref,
                      string entry,
                      shared_ptr<Reference> target) noexcept {
  ASSERT(ref->isResolved()) << "Cannot remove an entry from an unresolved directory";
  ASSERT(target->isResolved()) << "Cannot link an unresolved reference into a directory";

  ref->getArtifact()->addEntry(c, ref, entry, target);

  _trace->addStep(c, make_shared<Link>(ref, entry, target));
}

// Command c removes an entry from a directory
void Build::traceUnlink(shared_ptr<Command> c, shared_ptr<Reference> ref, string entry) noexcept {
  ASSERT(ref->isResolved()) << "Cannot remove an entry from an unresolved directory";

  ref->getArtifact()->removeEntry(c, ref, entry);

  _trace->addStep(c, make_shared<Unlink>(ref, entry));
}

// This command launches a child command
void Build::traceLaunch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept {
  if (options::print_on_run) cout << child->getShortName(options::command_length) << endl;

  // The child command depends on all current versions of the artifacts in its fd table
  for (auto& [index, desc] : child->getInitialFDs()) {
    desc.getReference()->getArtifact()->needsCurrentVersions(child);
  }

  child->setExecuted();

  _trace->addStep(c, make_shared<Launch>(child));
}

// This command joined with a child command
void Build::traceJoin(shared_ptr<Command> c, shared_ptr<Command> child, int exit_status) noexcept {
  LOG << c << " joined command " << child << " with exit status " << exit_status;

  // Save the exit status in the child
  child->setExitStatus(exit_status);

  // Add a join action to this command's steps
  _trace->addStep(c, make_shared<Join>(child, exit_status));
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
    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run)
      cout << child->getShortName(options::command_length) << endl;

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
      o << "  " << c->getShortName(options::command_length) << endl;
    }

  } else {
    o << "No commands to rerun" << endl;
  }

  return o;
}
