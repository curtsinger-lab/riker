#include "Build.hh"

#include <iostream>
#include <memory>
#include <ostream>

#include "artifacts/Artifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "build/AccessFilter.hh"
#include "build/Env.hh"
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
      // No. Emulate the step, which will also add it into the new trace
      step->emulate(cmd, *this);
    }
  }

  // Wait for all remaining processes to exit
  _tracer.wait();

  // Ask the environment to check remaining artifacts for changes, and to save metadata and
  // fingerprints for artifacts that were created during the build
  _env.finalize();
}

/************************ Command Tracing and Emulation ************************/

// Command c issued a pipe reference while being traced
shared_ptr<Pipe> Build::pipe(shared_ptr<Command> c, shared_ptr<Pipe> emulating) noexcept {
  auto ref = emulating;
  if (!emulating) ref = make_shared<Pipe>();
  ref->resolvesTo(_env.getPipe(c));
  _trace->addStep(c, ref);
  return ref;
}

// Command c accesses a path
shared_ptr<Access> Build::access(shared_ptr<Command> c,
                                 shared_ptr<Access> base,
                                 fs::path path,
                                 AccessFlags flags,
                                 shared_ptr<Access> emulating) noexcept {
  // Get a reference, either using the existing one we are emulating, or creating a new one
  auto ref = emulating;
  if (!emulating) ref = make_shared<Access>(base, path, flags);

  // Resolve the reference
  ref->resolvesTo(_env.resolveRef(c, ref));

  // If the access is being emulated, check the result
  if (emulating && ref->getResolution() != ref->getExpectedResult()) {
    observeCommandChange(c, emulating);
  }

  // Add the reference to the new build trace
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

// Command c accesses metadata during emulation
void Build::emulateMetadataMatch(shared_ptr<Command> c, shared_ptr<MetadataMatch> step) noexcept {
  auto ref = step->getReference();
  auto version = step->getVersion();

  // If the reference does not resolve, report a change
  if (!ref->getResolution()) return;

  // Get the latest metadata version. The returned version will be nullptr if no check is
  // necessary.
  const auto& v = ref->getArtifact()->accessMetadata(c, ref, InputType::Accessed);

  // If a version was returned and it doesn't match the expected version, report a mismatch
  if (v && !v->matches(version)) {
    observeMismatch(c, ref->getArtifact(), v, version);
  }

  // Report the read
  _metadata_filter.read(c.get(), ref);

  _trace->addStep(c, step);
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

// Command c sets metadata during emulation
void Build::emulateSetMetadata(shared_ptr<Command> c, shared_ptr<SetMetadata> step) noexcept {
  auto ref = step->getReference();
  auto version = step->getVersion();

  // If the reference did not resolve, report a change
  if (!ref->getResolution()) return;

  // Add the assigned version to the artifact
  ref->getArtifact()->setMetadata(c, ref, version);

  // Report the write
  _metadata_filter.write(c.get(), ref, version);

  _trace->addStep(c, step);
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

// Command c accesses file content while being emulated
void Build::emulateContentsMatch(shared_ptr<Command> c, shared_ptr<ContentsMatch> step) noexcept {
  auto ref = step->getReference();
  auto version = step->getVersion();

  // If the reference did not resolve, report a change
  if (!ref->getResolution()) return;

  // Get the latest content version. The returned version will be nullptr if no check is
  // necessary.
  const auto& v = ref->getArtifact()->accessContents(c, ref);

  // If a version was returned and it doesn't match the expected version, report a mismatch
  if (v && !v->matches(version)) {
    observeMismatch(c, ref->getArtifact(), v, version);
  }

  // Report the read
  _content_filter.read(c.get(), ref);

  _trace->addStep(c, step);
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

// Command c sets file content while being emulated
void Build::emulateSetContents(shared_ptr<Command> c, shared_ptr<SetContents> step) noexcept {
  auto ref = step->getReference();
  auto version = step->getVersion();

  // If the reference did not resolve, report a change
  if (!ref->getResolution()) return;

  // Add the assigned version to the artifact
  ref->getArtifact()->setContents(c, ref, version);

  // Report the write
  _content_filter.write(c.get(), ref, version);

  _trace->addStep(c, step);
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

// Command c accesses the contents of a symlink while being emulated
void Build::emulateSymlinkMatch(shared_ptr<Command> c, shared_ptr<SymlinkMatch> step) noexcept {
  auto ref = step->getReference();
  auto version = step->getVersion();

  // If the reference did not resolve, report a change
  if (!ref->getResolution()) return;

  // Get the latest symlink version
  const auto& v = ref->getArtifact()->readlink(c, InputType::Accessed);

  // If the returned version doesn't match the expected version, report a mismatch
  if (!v->matches(version)) {
    observeMismatch(c, ref->getArtifact(), v, version);
  }

  _trace->addStep(c, step);
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

// An emulated command removes an entry from a directory
void Build::emulateLink(shared_ptr<Command> c, shared_ptr<Link> step) noexcept {
  auto ref = step->getReference();
  auto entry = step->getEntry();
  auto target = step->getTarget();

  // If the reference did not resolve, report a change
  if (!ref->getResolution()) return;

  // Check the target reference as well
  if (!target->getResolution()) return;

  // Perform the unlink
  ref->getArtifact()->addEntry(c, ref, entry, target);

  _trace->addStep(c, step);
}

// Command c removes an entry from a directory
void Build::traceUnlink(shared_ptr<Command> c, shared_ptr<Reference> ref, string entry) noexcept {
  ASSERT(ref->isResolved()) << "Cannot remove an entry from an unresolved directory";

  ref->getArtifact()->removeEntry(c, ref, entry);

  _trace->addStep(c, make_shared<Unlink>(ref, entry));
}

// An emulated command removes an entry from a directory
void Build::emulateUnlink(shared_ptr<Command> c, shared_ptr<Unlink> step) noexcept {
  auto ref = step->getReference();
  auto entry = step->getEntry();

  // If the reference did not resolve, report a change
  if (!ref->getResolution()) return;

  // Perform the unlink
  ref->getArtifact()->removeEntry(c, ref, entry);

  _trace->addStep(c, step);
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

// An emulated command launches a child command
void Build::emulateLaunch(shared_ptr<Command> c, shared_ptr<Launch> step) noexcept {
  auto cmd = step->getCommand();

  // The child command depends on all current versions of the artifacts in its fd table
  for (auto& [index, desc] : cmd->getInitialFDs()) {
    desc.getReference()->getArtifact()->needsCurrentVersions(cmd);
  }

  // If this child hasn't run before, let the observers know
  if (!cmd->hasExecuted()) {
    for (const auto& o : _observers) {
      o->commandNeverRun(cmd);
    }
  }

  // Inform observers of the launch action
  for (const auto& o : _observers) {
    o->launch(c, cmd);
  }

  // If the child command must be rerun, start it in the tracer now
  if (checkRerun(cmd)) {
    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run)
      cout << cmd->getShortName(options::command_length) << endl;

    // Actually run the command, unless this is a dry run
    if (!options::dry_run) {
      // Start the command in the tracer
      _running[cmd] = _tracer.start(cmd);
      cmd->setExecuted();
    }
  }

  _trace->addStep(c, step);
}

// This command joined with a child command
void Build::join(shared_ptr<Command> c,
                 shared_ptr<Command> child,
                 int exit_status,
                 shared_ptr<Join> emulating) noexcept {
  LOG << c << " joined command " << child << " with exit status " << exit_status;

  if (emulating) {
    // If the command is in the rerun set, tell the tracer to wait for it
    if (checkRerun(child)) {
      INFO << "Waiting for process running " << child;
      _tracer.wait(_running[child]);
    }

    // Did the child command's exit status match the expected result?
    if (child->getExitStatus() != exit_status) {
      observeCommandChange(c, emulating);
    }

    // Add the emulated step to the new trace
    _trace->addStep(c, emulating);

  } else {
    // Save the exit status in the child
    child->setExitStatus(exit_status);

    // Add a join action to this command's steps
    _trace->addStep(c, make_shared<Join>(child, exit_status));
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
