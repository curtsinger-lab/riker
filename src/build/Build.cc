#include "Build.hh"

#include <iostream>
#include <memory>
#include <ostream>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "artifacts/SymlinkArtifact.hh"
#include "build/Env.hh"
#include "build/RebuildPlan.hh"
#include "build/Resolution.hh"
#include "core/IR.hh"
#include "tracing/Process.hh"
#include "tracing/Tracer.hh"
#include "ui/options.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::cout;
using std::endl;
using std::ostream;
using std::shared_ptr;

tuple<shared_ptr<Trace>, shared_ptr<Env>> Build::run() noexcept {
  // Resolve all the initial references in the trace (root, cwd, stdin, stdout, etc.)
  _trace->resolveRefs(*this, _env);

  // Emulate steps until we hit the end of the trace
  runSteps();

  // Wait for all remaining processes to exit
  _tracer.wait();

  // Compare the final state of all artifacts to the actual filesystem
  _env->getRootDir()->checkFinalState(*this, "/");

  return {_trace, _env};
}

void Build::runSteps() noexcept {
  while (!_steps.empty()) {
    // Take the first step from the list
    auto [cmd, step] = _steps.front();
    _steps.pop_front();

    // Can we emulate the command that created this IR step?
    if (_plan.canEmulate(cmd)) {
      // Yes. Call its emulate method
      step->emulate(cmd, *this);
    }
  }
}

/************************ Observer Implementation ************************/

// Inform observers that a command has never run
void Build::observeCommandNeverRun(shared_ptr<Command> c) const noexcept {
  for (const auto& o : _observers) o->commandNeverRun(c);
}

// Inform observers that a parent command launched a child command
void Build::observeLaunch(shared_ptr<Command> parent, shared_ptr<Command> child) const noexcept {
  for (const auto& o : _observers) o->launch(parent, child);
}

// Inform observers that command c modified artifact a, creating version v
void Build::observeOutput(shared_ptr<Command> c,
                          shared_ptr<Artifact> a,
                          shared_ptr<Version> v) const noexcept {
  for (const auto& o : _observers) o->output(c, a, v);
}

// Inform observers that command c  accessed version v of artifact a
void Build::observeInput(shared_ptr<Command> c,
                         shared_ptr<Artifact> a,
                         shared_ptr<Version> v,
                         InputType t) noexcept {
  // Is this input accessing the last write we observed? We care specifically about
  auto& [write_command, write_ref, write_version] = _last_write;
  if (v == write_version && c != write_command) {
    // Yes. The version is now accessed, so clear the last write
    _last_write = {nullptr, nullptr, nullptr};
  }

  if (_plan.mustRerun(c) && !v->isCommitted()) {
    // The command c is running, and needs uncommitted version v. We can commit it now
    ASSERT(a->canCommit(v)) << "Running command " << c << " depends on an uncommittable version "
                            << v << " of " << a;
    LOG(exec) << "Committing " << v << " to " << a << " on demand";
    a->commit(v);
  }

  for (const auto& o : _observers) o->input(c, a, v, t);
}

// Inform observers that command c did not find the expected version in artifact a
// Instead of version `expected`, the command found version `observed`
void Build::observeMismatch(shared_ptr<Command> c,
                            shared_ptr<Artifact> a,
                            shared_ptr<Version> observed,
                            shared_ptr<Version> expected) const noexcept {
  for (const auto& o : _observers) o->mismatch(c, a, observed, expected);
}

// Inform observers that a given command's IR action would detect a change in the build env
void Build::observeCommandChange(shared_ptr<Command> c, shared_ptr<const Step> s) const noexcept {
  for (const auto& o : _observers) o->commandChanged(c, s);
}

// Inform observers that the version of an artifact produced during the build does not match the
// on-disk version.
void Build::observeFinalMismatch(shared_ptr<Artifact> a,
                                 shared_ptr<Version> produced,
                                 shared_ptr<Version> ondisk) const noexcept {
  for (const auto& o : _observers) o->finalMismatch(a, produced, ondisk);
}

/******** Reference Resolution *********/

RefResult Build::saveResult(shared_ptr<Command> cmd, Resolution result) noexcept {
  size_t index = _ref_results[cmd].size();
  _ref_results[cmd].push_back(result);
  return RefResult(cmd, index);
}

Resolution Build::getResult(RefResult r) noexcept {
  return _ref_results[r.getCommand()][r.getIndex()];
}

/************************ Command Tracing and Emulation ************************/

// Command c creates a new pipe
shared_ptr<Pipe> Build::pipe(shared_ptr<Command> c, shared_ptr<Pipe> emulating) noexcept {
  // Use or create a trace step
  auto ref = emulating;
  if (!emulating) ref = make_shared<Pipe>();

  // Add the step to the output trace
  _trace->addStep(c, ref, static_cast<bool>(emulating));

  // Create a pipe and save the resolved result
  auto result = _env->getPipe(*this, c);
  ref->resolvesTo(result);
  saveResult(c, result);

  return ref;
}

// Command c creates a new file
shared_ptr<File> Build::file(shared_ptr<Command> c,
                             mode_t mode,
                             shared_ptr<File> emulating) noexcept {
  // Use or create a trace step
  auto ref = emulating;
  if (!emulating) ref = make_shared<File>(mode);

  // Add the step to the output trace
  _trace->addStep(c, ref, static_cast<bool>(emulating));

  // Create a file and save the resolved result
  auto result = _env->createFile(*this, c, mode, !emulating);
  ref->resolvesTo(result);
  saveResult(c, result);

  return ref;
}

// Command c creates a new symbolic link
shared_ptr<Symlink> Build::symlink(shared_ptr<Command> c,
                                   fs::path target,
                                   shared_ptr<Symlink> emulating) noexcept {
  // Use or create a trace step
  auto ref = emulating;
  if (!emulating) ref = make_shared<Symlink>(target);

  // Add the step to the output trace
  _trace->addStep(c, ref, static_cast<bool>(emulating));

  // Create a symlink and save the resolved result
  auto result = _env->getSymlink(*this, c, target, !emulating);
  ref->resolvesTo(result);
  saveResult(c, result);

  return ref;
}

// Command c creates a new directory
shared_ptr<Dir> Build::dir(shared_ptr<Command> c, mode_t mode, shared_ptr<Dir> emulating) noexcept {
  // Use or create a trace step
  auto ref = emulating;
  if (!emulating) ref = make_shared<Dir>(mode);

  // Add the step to the output trace
  _trace->addStep(c, ref, static_cast<bool>(emulating));

  // Create a directory and save the resolved result
  auto result = _env->getDir(*this, c, mode, !emulating);
  ref->resolvesTo(result);
  saveResult(c, result);

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

  // Add the reference to the new build trace
  _trace->addStep(c, ref, static_cast<bool>(emulating));

  // Resolve the reference
  auto result =
      base->getArtifact()->resolve(*this, c, nullptr, path.begin(), path.end(), ref, !emulating);

  // Save the result of the resolution
  ref->resolvesTo(result);
  saveResult(c, result);

  return ref;
}

// Command c expects a reference to resolve with a specific result
void Build::expectResult(shared_ptr<Command> c,
                         shared_ptr<Ref> ref,
                         int expected,
                         shared_ptr<ExpectResult> emulating) noexcept {
  // Use or create an IR step
  auto step = emulating;
  if (!emulating) step = make_shared<ExpectResult>(ref, expected);

  // Add the step to the output trace
  _trace->addStep(c, step, static_cast<bool>(emulating));

  // Does the resolved reference match the expected result?
  if (emulating && ref->getResolution() != expected) {
    observeCommandChange(c, emulating);
  }
}

// Command c accesses an artifact's metadata
void Build::matchMetadata(shared_ptr<Command> c,
                          shared_ptr<Ref> ref,
                          shared_ptr<MetadataVersion> expected,
                          shared_ptr<MatchMetadata> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << c << " tried to access metadata through unresolved reference " << ref;

    // Report the change
    observeCommandChange(c, emulating);

    // Add the step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating, true);
    return;
  }

  // Are we emulating this operation?
  if (emulating) {
    // Yes. We need an expected version to check for
    ASSERT(expected) << "An emulated MatchMetadata step did not provide expected metadata";

    // Perform the comparison
    ref->getArtifact()->matchMetadata(*this, c, expected);

    // Record the emulated trace step
    _trace->addStep(c, emulating, true);

  } else {
    // No. This is a traced command

    // If we don't have an expected version already, get one from the current state
    if (!expected) expected = ref->getArtifact()->getMetadata(*this, c, InputType::Accessed);

    ASSERT(expected) << "Unable to get current metadata from " << ref->getArtifact();

    // If a different command created this version, fingerprint it for later comparison
    auto creator = expected->getCreator();
    if (!creator || creator != c) {
      // We can only take a fingerprint with a path
      if (auto access = ref->as<Access>()) expected->fingerprint(access->getFullPath());
    }

    // Add a match step to the trace
    _trace->addStep(c, make_shared<MatchMetadata>(ref, expected), false);
  }
}

// Command c accesses an artifact's content
void Build::matchContent(shared_ptr<Command> c,
                         shared_ptr<Ref> ref,
                         shared_ptr<Version> expected,
                         shared_ptr<MatchContent> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << c << " tried to access content through unresolved reference " << ref;

    // Report the change
    observeCommandChange(c, emulating);

    // Add the step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating, true);
    return;
  }

  // Are we emulating this operation?
  if (emulating) {
    // Yes. We need an expected version to check for
    ASSERT(expected) << "An emulated MatchContent step did not provide an expected version";

    // Perform the comparison
    ref->getArtifact()->matchContent(*this, c, expected);

    // Record the emulated trace step
    _trace->addStep(c, emulating, true);

  } else {
    // No. This is a traced command

    // If we don't have an expected version already, get one from the current state
    if (!expected) expected = ref->getArtifact()->getContent(*this, c, InputType::Accessed);

    ASSERT(expected) << "Unable to get content from " << ref->getArtifact();

    // If this access is from the same command and reference as the last write, and the versions are
    // the same, skip the trace step
    if (_last_write == tuple{c, ref, expected}) {
      LOG(exec) << "Omitting " << c << " access to " << ref->getArtifact();
      return;
    }

    // If a different command created this version, fingerprint it for later comparison
    auto creator = expected->getCreator();
    if (!creator || creator != c) {
      // We can only take a fingerprint with a path
      if (auto access = ref->as<Access>()) expected->fingerprint(access->getFullPath());
    }

    // Add a match step to the trace
    _trace->addStep(c, make_shared<MatchContent>(ref, expected), false);
  }
}

// Command c modifies an artifact
void Build::updateMetadata(shared_ptr<Command> c,
                           shared_ptr<Ref> ref,
                           shared_ptr<MetadataVersion> written,
                           shared_ptr<UpdateMetadata> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << c << " tried to write metadata through an unresolved reference " << ref;

    // Record the change
    observeCommandChange(c, emulating);

    // Add the IR step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating, true);
    return;
  }

  // Are we emulating this command?
  if (emulating) {
    // Yes. We should have an existing version to write
    ASSERT(written) << "An emulated command is writing an unspecified version to an artifact";

    // Make sure this version is NOT marked as committed
    written->setCommitted(false);

    // Mark the version as created by the calling command. This field is transient, so we have to
    // apply it on ever run
    written->createdBy(c);

    // Apply the write
    ref->getArtifact()->updateMetadata(*this, c, written);

    // Add this write to the trace
    _trace->addStep(c, emulating, true);

  } else {
    // No. This is a traced operation

    // Update the artifact and hold on to the metadata version it returns.
    written = ref->getArtifact()->updateMetadata(*this, c, written);

    // The calling command created this version
    written->createdBy(c);

    // This apply operation was traced, so the written version is committed
    written->setCommitted();

    // Add a new trace step
    _trace->addStep(c, make_shared<UpdateMetadata>(ref, written), false);
  }
}

// Command c modifies an artifact
void Build::updateContent(shared_ptr<Command> c,
                          shared_ptr<Ref> ref,
                          shared_ptr<Version> written,
                          shared_ptr<UpdateContent> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << c << " tried to write through an unresolved reference " << ref;

    // Record the change
    observeCommandChange(c, emulating);

    // Add the IR step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating, true);
    return;
  }

  // Are we emulating this command?
  if (emulating) {
    // Yes. We should have an existing version to write
    ASSERT(written) << "An emulated command is writing an unspecified version to an artifact";

    // Make sure this version is NOT marked as committed
    written->setCommitted(false);

    // Mark the version as created by the calling command. This field is transient, so we have to
    // apply it on ever run
    written->createdBy(c);

    // Apply the write
    written->applyTo(*this, c, ref->getArtifact());

    // Save the last write
    _last_write = {c, ref, written};

    // Add this write to the trace
    _trace->addStep(c, emulating, true);

  } else {
    // No. This is a traced operation

    // Was the last write from the same command and reference?
    auto [last_write_command, last_write_ref, last_write_version] = _last_write;
    if (c == last_write_command && ref == last_write_ref && !last_write_version->hasFingerprint()) {
      // Yes. We can skip the trace step.
      LOG(exec) << "Skipping " << c << " write to " << ref->getArtifact();
      return;
    }

    // If a written version was not provided, ask the artifact for one
    if (!written) written = ref->getArtifact()->createContentVersion();

    // This apply operation was traced, so the written version is committed
    written->setCommitted();

    // The calling command created this version
    written->createdBy(c);

    // Update the artifact's content
    written->applyTo(*this, c, ref->getArtifact());

    _last_write = {c, ref, written};

    // Add a new trace step
    _trace->addStep(c, make_shared<UpdateContent>(ref, written), false);
  }
}

// This command launches a child command
void Build::launch(shared_ptr<Command> c,
                   shared_ptr<Command> child,
                   shared_ptr<Launch> emulating) noexcept {
  LOG(exec) << c << " launching " << child;

  // If we're emulating the launch of an unexecuted command, notify observers
  if (emulating && !child->hasExecuted()) {
    observeCommandNeverRun(child);
  }

  // Inform observers of the launch
  observeLaunch(c, child);

  // Is the child command being executed? If the parent is executing or the child is marked, yes.
  if (!emulating || _plan.mustRerun(child)) {
    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run) {
      cout << child->getShortName(options::command_length) << endl;
    }

    // Is this a real execution and not a dry run?
    if (!options::dry_run) {
      // Yes. The child command will be executed by this build.
      child->setExecuted();

      // The child command requires that its working directory exists
      child->getInitialWorkingDir()->getArtifact()->mustExist(*this, child);

      // The executable must be fully committed
      child->getExecutable()->getArtifact()->commitAll();

      // The child command also depends on the artifacts reachable through its initial FDs
      for (auto& [index, desc] : child->getInitialFDs()) {
        // TODO: Check pipes as well. Skipping non-path references for now
        if (auto access = desc.getRef()->as<Access>()) {
          access->getArtifact()->commitAll();
        }
      }

      // If we are emulating the launch of the child command, tell the tracer to start it
      if (emulating) {
        _running[child] = _tracer.start(child);
      }
    }
  }

  // Make sure we have a launch IR step to record
  auto step = emulating;
  if (!step) step = make_shared<Launch>(child);

  // Add the command to the trace
  _trace->addCommand(child);

  // Add the launch step to the trace
  _trace->addStep(c, step, static_cast<bool>(emulating));
}

// This command joined with a child command
void Build::join(shared_ptr<Command> c,
                 shared_ptr<Command> child,
                 int exit_status,
                 shared_ptr<Join> emulating) noexcept {
  if (emulating) {
    // If the command is in the rerun set, tell the tracer to wait for it
    if (isRunning(child)) {
      _tracer.wait(_running[child]);
    }

    // Did the child command's exit status match the expected result?
    if (child->getExitStatus() != exit_status) {
      observeCommandChange(c, emulating);
    }

    // Add the emulated step to the new trace
    _trace->addStep(c, emulating, true);

  } else {
    // Save the exit status in the child (TODO: Remove this once we know Build::exit works)
    child->setExitStatus(exit_status);

    // Add a join action to this command's steps
    _trace->addStep(c, make_shared<Join>(child, exit_status), false);
  }
}

void Build::exit(shared_ptr<Command> c, int exit_status, shared_ptr<Exit> emulating) noexcept {
  // Record that the command has exited
  _exited.insert(c);

  // Save the exit status for this command (TODO: remove once EXIT changes are supported for real)
  c->setExitStatus(exit_status);

  if (emulating) {
    // Add the emulated step to the new trace
    _trace->addStep(c, emulating, true);

  } else {
    // Add an exit action to this command's steps
    _trace->addStep(c, make_shared<Exit>(exit_status), false);
  }
}
