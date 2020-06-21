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

// Command c accesses an artifact's metadata
void Build::metadataMatch(shared_ptr<Command> c,
                          shared_ptr<Reference> ref,
                          shared_ptr<MetadataVersion> expected,
                          shared_ptr<MetadataMatch> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << "A traced command accesses metadata through an unresolved reference";

    // Report the change
    observeCommandChange(c, emulating);

    // Add the step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating);
    return;
  }

  // If this command does not need to trace the metadata access, return immediately
  if (!_metadata_filter.readRequired(c.get(), ref)) return;

  // Get a metadata version from the artifact
  auto observed = ref->getArtifact()->accessMetadata(c, ref, InputType::Accessed);

  // Is this step being emulated or run?
  if (emulating) {
    // If we are emulating, compare the observed version with the expected version
    if (!observed->matches(expected)) observeMismatch(c, ref->getArtifact(), observed, expected);

    // Add the existing MetadataMatch step to the new trace
    _trace->addStep(c, emulating);

  } else {
    // If we are tracing, we need to access metadata and save it for later comparison
    // We can skip the fingerprint if this command also created the version
    if (observed->getCreator() != c) observed->fingerprint(ref);

    // Add a new MetadataMatch step to the trace, which expects to find the version we observed
    _trace->addStep(c, make_shared<MetadataMatch>(ref, observed));
  }

  // Report the read
  _metadata_filter.read(c.get(), ref);
}

// Command c sets sets the metadata for an artifact
void Build::setMetadata(shared_ptr<Command> c,
                        shared_ptr<Reference> ref,
                        shared_ptr<MetadataVersion> written,
                        shared_ptr<SetMetadata> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << "A traced command tried to set metadata through an unresolved reference";

    // Record the change
    observeCommandChange(c, emulating);

    // Add the IR step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating);
    return;
  }

  // Do we have to log this write?
  if (!_metadata_filter.writeRequired(c.get(), ref)) return;

  // Apply the new version to the artifact's metadata.
  // The written version will be null when tracing, so save the returned version
  written = ref->getArtifact()->setMetadata(c, ref, written);

  // If we're emulating, add the existing IR step to the trace. Otherwise record a new IR step.
  if (emulating) {
    _trace->addStep(c, emulating);
  } else {
    _trace->addStep(c, make_shared<SetMetadata>(ref, written));
  }

  // Report the write
  _metadata_filter.write(c.get(), ref, written);
}

// Command c accesses an artifact's contents
void Build::contentsMatch(shared_ptr<Command> c,
                          shared_ptr<Reference> ref,
                          shared_ptr<ContentVersion> expected,
                          shared_ptr<ContentsMatch> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << "A traced command accesses an artifact through an unresolved reference";

    // Report the change
    observeCommandChange(c, emulating);

    // Add the step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating);
    return;
  }

  // If this command does not need to trace the metadata access, return immediately
  if (!_content_filter.readRequired(c.get(), ref)) return;

  // Get a metadata version from the artifact
  auto observed = ref->getArtifact()->accessContents(c, ref);

  // Is this step being emulated or run?
  if (emulating) {
    // If we are emulating, compare the observed version with the expected version
    if (!observed->matches(expected)) observeMismatch(c, ref->getArtifact(), observed, expected);

    // Add the existing MetadataMatch step to the new trace
    _trace->addStep(c, emulating);

  } else {
    // If we are tracing, we need to access the artifact and fingerprint the version for comparison.
    // We can skip the fingerprint if this command also created the version
    if (observed->getCreator() != c) observed->fingerprint(ref);

    // Add a new MetadataMatch step to the trace, which expects to find the version we observed
    _trace->addStep(c, make_shared<ContentsMatch>(ref, observed));
  }

  // Report the read
  _content_filter.read(c.get(), ref);
}

// Command c sets the contents of an artifact
void Build::setContents(shared_ptr<Command> c,
                        shared_ptr<Reference> ref,
                        shared_ptr<ContentVersion> written,
                        shared_ptr<SetContents> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << "A traced command tried to set contents through an unresolved reference";

    // Record the change
    observeCommandChange(c, emulating);

    // Add the IR step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating);
    return;
  }

  // Do we have to log this write?
  if (!_content_filter.writeRequired(c.get(), ref)) return;

  // Apply the new version to the artifact's metadata.
  // The written version will be null when tracing, so save the returned version
  written = ref->getArtifact()->setContents(c, ref, written);

  // If we're emulating, add the existing IR step to the trace. Otherwise record a new IR step.
  if (emulating) {
    _trace->addStep(c, emulating);
  } else {
    _trace->addStep(c, make_shared<SetContents>(ref, written));
  }

  // Report the write
  _content_filter.write(c.get(), ref, written);
}

// Command c accesses the contents of a symlink while being traced
void Build::symlinkMatch(shared_ptr<Command> c,
                         shared_ptr<Reference> ref,
                         shared_ptr<SymlinkVersion> expected,
                         shared_ptr<SymlinkMatch> emulating) noexcept {
  // If the reference is not resolved, report a change
  if (!ref->isResolved()) {
    ASSERT(emulating) << "A traced command accessed an unresolved reference to a symlink";

    // Report the change
    observeCommandChange(c, emulating);

    // Add the step and return. Nothing else can be done because we have no reachable artifact.
    _trace->addStep(c, emulating);
    return;
  }

  // Make the access
  auto observed = ref->getArtifact()->readlink(c, InputType::Accessed);

  // Is this step being emulated or run?
  if (emulating) {
    // We are emulating this action, so compare the observed and expected versions
    if (!observed->matches(expected)) {
      observeMismatch(c, ref->getArtifact(), observed, expected);
    }

    // Add the existing IR step to the trace
    _trace->addStep(c, emulating);

  } else {
    // Save a fingerprint from the link so we can compare to this version later
    observed->fingerprint(ref);

    // Add a new step to the trace
    _trace->addStep(c, make_shared<SymlinkMatch>(ref, observed));
  }
}

// Command c adds an entry to a directory
void Build::link(shared_ptr<Command> c,
                 shared_ptr<Reference> dir_ref,
                 string entry,
                 shared_ptr<Reference> target,
                 shared_ptr<Link> emulating) noexcept {
  // If either reference is unresolved, something must have changed
  if (!dir_ref->isResolved() || !target->isResolved()) {
    ASSERT(emulating) << "A traced command attempted to use an unresolved reference";

    // Report the change
    observeCommandChange(c, emulating);

    // Add the step to the trace, then return.
    _trace->addStep(c, emulating);
    return;
  }

  // Perform the link on the directory artifact
  dir_ref->getArtifact()->addEntry(c, dir_ref, entry, target);

  // Make sure we have a trace step to record
  auto step = emulating;
  if (!step) step = make_shared<Link>(dir_ref, entry, target);

  // Add the trace step
  _trace->addStep(c, step);
}

// Command c removes an entry from a directory
void Build::unlink(shared_ptr<Command> c,
                   shared_ptr<Reference> dir_ref,
                   string entry,
                   shared_ptr<Unlink> emulating) noexcept {
  // If the directory reference is unresolved, something must have changed
  if (!dir_ref->isResolved()) {
    ASSERT(emulating) << "A traced command attempted to use an unresolved reference";

    // Report the change
    observeCommandChange(c, emulating);

    // Add the step to the trace, then return.
    _trace->addStep(c, emulating);
    return;
  }

  // Perform the unlink on the directory artifact
  dir_ref->getArtifact()->removeEntry(c, dir_ref, entry);

  // Make sure we have a step to record
  auto step = emulating;
  if (!step) step = make_shared<Unlink>(dir_ref, entry);

  _trace->addStep(c, step);
}

// This command launches a child command
void Build::launch(shared_ptr<Command> c,
                   shared_ptr<Command> child,
                   shared_ptr<Launch> emulating) noexcept {
  // The child command depends on all current versions of the artifacts in its fd table
  for (auto& [index, desc] : child->getInitialFDs()) {
    desc.getReference()->getArtifact()->needsCurrentVersions(child);
  }

  // If we're emulating the launch of an unexecuted command, notify observers
  if (emulating && !child->hasExecuted()) {
    // If we're emulating, we need to let the observers know if the child has not been run before
    for (const auto& o : _observers) {
      o->commandNeverRun(child);
    }
  }

  // Inform observers of the launch
  for (const auto& o : _observers) {
    o->launch(c, child);
  }

  // Should this command rerun? Yes, if we're either tracing the launch, or the command is marked
  if (!emulating || checkRerun(child)) {
    // Show the command if printing is on, or if this is a dry run
    if (_print_on_run || _dry_run) {
      cout << child->getShortName(options::command_length) << endl;
    }

    // Is this a real execution and not a dry run?
    if (!_dry_run) {
      // Yes. The child will be executed
      child->setExecuted();

      // If we are emulating the launch of the child command, tell the tracer to start it
      if (emulating) _running[child] = _tracer.start(child);
    }
  }

  // Make sure we have a launch IR step to record
  auto step = emulating;
  if (!step) step = make_shared<Launch>(child);

  // Add the launch step to the trace
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
