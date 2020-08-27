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
  // Emulate steps until we hit the end of the trace
  for (auto& [cmd, step] : _steps) {
    // Can we emulate the command that created this IR step?
    if (_plan.canEmulate(cmd)) {
      // Yes. Call its emulate method
      step->emulate(cmd, *this);
    }
  }

  // Wait for all remaining processes to exit
  _tracer.wait();

  // Compare the final state of all artifacts to the actual filesystem
  _env->getRootDir()->checkFinalState(*this, "/");

  return {_trace, _env};
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

/************************ Create References ************************/

/// A command is issuing a reference to a special artifact (e.g. stdin, stdout, root dir)
shared_ptr<SpecialRef> Build::createSpecialRef(SpecialRef::Entity entity) noexcept {
  return make_shared<SpecialRef>(entity);
}

/// A command references a new anonymous pipe
shared_ptr<PipeRef> Build::createPipeRef() noexcept {
  return make_shared<PipeRef>();
}

/// A command references a new anonymous file
shared_ptr<FileRef> Build::createFileRef(mode_t mode) noexcept {
  return make_shared<FileRef>(mode);
}

/// A command references a new anonymous symlink
shared_ptr<SymlinkRef> Build::createSymlinkRef(fs::path target) noexcept {
  return make_shared<SymlinkRef>(target);
}

/// A command references a new anonymous directory
shared_ptr<DirRef> Build::createDirRef(mode_t mode) noexcept {
  return make_shared<DirRef>(mode);
}

/// A command makes a reference with a path
shared_ptr<PathRef> Build::createPathRef(shared_ptr<Resolve> base,
                                         fs::path path,
                                         AccessFlags flags) noexcept {
  return make_shared<PathRef>(base, path, flags);
}

/************************ Resolve References ************************/

// Command c is issuing a special reference
Resolution Build::resolveSpecialRef(shared_ptr<Command> c,
                                    SpecialRef::Entity entity,
                                    shared_ptr<Resolve> result,
                                    bool committed) noexcept {
  switch (entity) {
    case SpecialRef::stdin:
      return _env->getStdin(*this, c);

    case SpecialRef::stdout:
      return _env->getStdout(*this, c);

    case SpecialRef::stderr:
      return _env->getStderr(*this, c);

    case SpecialRef::root:
      return _env->getRootDir();

    case SpecialRef::cwd:
      auto cwd_path = fs::current_path().relative_path();
      auto res = _env->getRootDir()->resolve(*this, c, nullptr, cwd_path.begin(), cwd_path.end(),
                                             AccessFlags{.x = true}, result, false);
      ASSERT(res) << "Failed to resolve current working directory";
      res->setName(".");
      return res;
  }
}

// Command c creates a new pipe
Resolution Build::resolvePipeRef(shared_ptr<Command> c,
                                 shared_ptr<Resolve> result,
                                 bool committed) noexcept {
  return _env->getPipe(*this, c);
}

// Command c creates a new file
Resolution Build::resolveFileRef(shared_ptr<Command> c,
                                 mode_t mode,
                                 shared_ptr<Resolve> result,
                                 bool committed) noexcept {
  return _env->createFile(*this, c, mode, committed);
}

// Command c creates a new symbolic link
Resolution Build::resolveSymlinkRef(shared_ptr<Command> c,
                                    fs::path target,
                                    shared_ptr<Resolve> result,
                                    bool committed) noexcept {
  return _env->getSymlink(*this, c, target, committed);
}

// Command c creates a new directory
Resolution Build::resolveDirRef(shared_ptr<Command> c,
                                mode_t mode,
                                shared_ptr<Resolve> result,
                                bool committed) noexcept {
  return _env->getDir(*this, c, mode, committed);
}

// Command c accesses a path
Resolution Build::resolvePathRef(shared_ptr<Command> c,
                                 shared_ptr<Resolve> base,
                                 fs::path path,
                                 AccessFlags flags,
                                 shared_ptr<Resolve> result,
                                 bool committed) noexcept {
  ASSERT(base->isResolved()) << "Cannot resolve a path relative to an unresolved base reference.";
  return base->getArtifact()->resolve(*this, c, nullptr, path.begin(), path.end(), flags, result,
                                      committed);
}

/************************ Emulate IR Steps ************************/

// Command c resolves a reference
shared_ptr<Resolve> Build::emulateResolve(shared_ptr<Command> c,
                                          shared_ptr<Ref> ref,
                                          shared_ptr<Resolve> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // Resolve the reference and save the result
  auto result = ref->resolve(c, *this, step, false);
  step->resolvesTo(result);

  return step;
}

// Command c expects a reference to resolve with a specific result
void Build::emulateExpectResult(shared_ptr<Command> c,
                                shared_ptr<Resolve> ref,
                                int expected,
                                shared_ptr<ExpectResult> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // Does the resolved reference match the expected result?
  if (ref->getResolution() != expected) {
    observeCommandChange(c, step);
  }
}

// Command c accesses an artifact's metadata
void Build::emulateMatchMetadata(shared_ptr<Command> c,
                                 shared_ptr<Resolve> ref,
                                 shared_ptr<MetadataVersion> expected,
                                 shared_ptr<MatchMetadata> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    // Report the change and return
    observeCommandChange(c, step);
    return;
  }

  // Perform the comparison
  ref->getArtifact()->matchMetadata(*this, c, expected);
}

// Command c accesses an artifact's content
void Build::emulateMatchContent(shared_ptr<Command> c,
                                shared_ptr<Resolve> ref,
                                shared_ptr<Version> expected,
                                shared_ptr<MatchContent> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    // Report the change and return
    observeCommandChange(c, step);
    return;
  }

  // Perform the comparison
  ref->getArtifact()->matchContent(*this, c, expected);
}

// Command c modifies an artifact
void Build::emulateUpdateMetadata(shared_ptr<Command> c,
                                  shared_ptr<Resolve> ref,
                                  shared_ptr<MetadataVersion> written,
                                  shared_ptr<UpdateMetadata> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    // Record the change and return
    observeCommandChange(c, step);
    return;
  }

  // Make sure this version is NOT marked as committed
  written->setCommitted(false);

  // Mark the version as created by the calling command. This field is transient, so we have to
  // apply it on ever run
  written->createdBy(c);

  // Apply the write
  ref->getArtifact()->updateMetadata(*this, c, written);
}

// Command c modifies an artifact
void Build::emulateUpdateContent(shared_ptr<Command> c,
                                 shared_ptr<Resolve> ref,
                                 shared_ptr<Version> written,
                                 shared_ptr<UpdateContent> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    // Record the change and return
    observeCommandChange(c, step);
    return;
  }

  // Make sure this version is NOT marked as committed
  written->setCommitted(false);

  // Mark the version as created by the calling command. This field is transient, so we have to
  // apply it on ever run
  written->createdBy(c);

  // Apply the write
  written->applyTo(*this, c, ref->getArtifact());

  // Save the last write
  _last_write = {c, ref, written};
}

// This command launches a child command
void Build::emulateLaunch(shared_ptr<Command> c,
                          shared_ptr<Command> child,
                          shared_ptr<Launch> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // Add the child to the trace
  _trace->addCommand(child);

  LOG(exec) << c << " launching " << child;

  // If we're emulating the launch of an unexecuted command, notify observers
  if (!child->hasExecuted()) observeCommandNeverRun(child);

  // Inform observers of the launch
  observeLaunch(c, child);

  // Does the child command need to be executed?
  if (_plan.mustRerun(child)) {
    // Show the command if printing is on, or if this is a dry run
    if (options::print_on_run || options::dry_run) {
      cout << child->getShortName(options::command_length) << endl;
    }

    // If this is a dry run, we're done emulating this step
    if (options::dry_run) return;

    // The child command will be executed by this build.
    child->setExecuted();

    // The child command requires that its working directory exists
    child->getInitialWorkingDir()->getArtifact()->mustExist(*this, child);

    // The executable must be fully committed
    child->getExecutable()->getArtifact()->commitAll();

    // The child command also depends on the artifacts reachable through its initial FDs
    for (auto& [index, desc] : child->getInitialFDs()) {
      auto artifact = desc.getRef()->getArtifact();

      // TODO: Handle pipes eventually. Just skip them for now
      if (artifact->as<PipeArtifact>()) continue;

      if (artifact->canCommitAll()) {
        artifact->commitAll();
      } else {
        WARN << "Launching " << child << " without committing referenced artifact " << artifact;
      }
    }

    // Start the child command in the tracer
    _running[child] = _tracer.start(child);
  }
}

// This command joined with a child command
void Build::emulateJoin(shared_ptr<Command> c,
                        shared_ptr<Command> child,
                        int exit_status,
                        shared_ptr<Join> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // If the child command is running in the tracer, wait for it
  if (isRunning(child)) _tracer.wait(_running[child]);

  // Did the child command's exit status match the expected result?
  if (child->getExitStatus() != exit_status) {
    observeCommandChange(c, step);
  }
}

void Build::emulateExit(shared_ptr<Command> c, int exit_status, shared_ptr<Exit> step) noexcept {
  // Add the emulated step to the output trace
  _trace->addEmulatedStep(c, step);

  // Record that the command has exited
  _exited.insert(c);

  // Save the exit status for this command (TODO: remove once EXIT changes are supported for real)
  c->setExitStatus(exit_status);
}

/************************ Trace IR Steps ************************/

// Command c resolves a reference
shared_ptr<Resolve> Build::traceResolve(shared_ptr<Command> c, shared_ptr<Ref> ref) noexcept {
  // Create an IR step and add it to the output trace
  auto step = make_shared<Resolve>(ref);
  _trace->addTracedStep(c, step);

  // Resolve the reference and save the result
  auto result = ref->resolve(c, *this, step, true);
  step->resolvesTo(result);

  return step;
}

// Command c expects a reference to resolve with a specific result
void Build::traceExpectResult(shared_ptr<Command> c,
                              shared_ptr<Resolve> ref,
                              int expected) noexcept {
  // Create an IR step and add it to the output trace
  auto step = make_shared<ExpectResult>(ref, expected);
  _trace->addTracedStep(c, step);
}

// Command c accesses an artifact's metadata
void Build::traceMatchMetadata(shared_ptr<Command> c, shared_ptr<Resolve> ref) noexcept {
  // Get the artifact whose metadata is being accessed
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to access metadata through unresolved reference " << ref;

  // Get the current metadata from the artifact
  auto expected = artifact->getMetadata(*this, c, InputType::Accessed);
  ASSERT(expected) << "Unable to get metadata from " << artifact;

  // Create an IR step and add it to the output trace
  auto step = make_shared<MatchMetadata>(ref, expected);
  _trace->addTracedStep(c, step);

  // If a different command created this version, fingerprint it for later comparison
  auto creator = expected->getCreator();
  if (!creator || creator != c) {
    // We can only take a fingerprint with a committed path
    auto path = artifact->getPath(false);
    if (path.has_value()) {
      expected->fingerprint(path.value());
    }
  }
}

// Command c accesses an artifact's content
void Build::traceMatchContent(shared_ptr<Command> c, shared_ptr<Resolve> ref) noexcept {
  // Get the artifact whose content is being accessed
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to access content through an unresolved reference " << ref;

  // Get the current content of the artifact
  auto expected = artifact->getContent(*this, c, InputType::Accessed);
  ASSERT(expected) << "Unable to get content from " << artifact;

  // If this access is from the same command and reference as the last write, and the versions are
  // the same, skip the trace step
  if (_last_write == tuple{c, ref, expected}) return;

  // Create an IR step and add it to the output trace
  auto step = make_shared<MatchContent>(ref, expected);
  _trace->addTracedStep(c, step);

  // If a different command created this version, fingerprint it for later comparison
  auto creator = expected->getCreator();
  if (!creator || creator != c) {
    // We can only take a fingerprint with a committed path
    auto path = artifact->getPath(false);
    if (path.has_value()) {
      expected->fingerprint(path.value());
    }
  }
}

// Command c modifies an artifact
void Build::traceUpdateMetadata(shared_ptr<Command> c, shared_ptr<Resolve> ref) noexcept {
  // Get the artifact whose metadata is being written
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to write metadata through an unresolved reference " << ref;

  // Record the update and get the written version
  auto written = artifact->updateMetadata(*this, c);
  ASSERT(written) << "Unable to get written metadata version from " << artifact;

  // Create an IR step and add it to the output trace
  auto step = make_shared<UpdateMetadata>(ref, written);
  _trace->addTracedStep(c, step);

  // The calling command created this version
  written->createdBy(c);

  // This apply operation was traced, so the written version is committed
  written->setCommitted();
}

// Command c modifies an artifact
void Build::traceUpdateContent(shared_ptr<Command> c,
                               shared_ptr<Resolve> ref,
                               shared_ptr<Version> written) noexcept {
  // Get the artifact whose content is being written
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to write content through an unresolved reference " << ref;

  // Was the last write from the same command and reference?
  auto [last_write_command, last_write_ref, last_write_version] = _last_write;
  if (c == last_write_command && ref == last_write_ref && !last_write_version->hasFingerprint()) {
    // Yes. We can skip the trace step.
    return;
  }

  // If a written version was not provided, ask the artifact for one
  if (!written) written = artifact->createContentVersion();
  ASSERT(written) << "Failed to get written version for " << artifact;

  // Create an IR step and add it to the output trace
  auto step = make_shared<UpdateContent>(ref, written);
  _trace->addTracedStep(c, step);

  // This apply operation was traced, so the written version is committed
  written->setCommitted();

  // The calling command created this version
  written->createdBy(c);

  // Update the artifact's content
  written->applyTo(*this, c, artifact);

  // Update the last write record
  _last_write = {c, ref, written};
}

// This command launches a child command
void Build::traceLaunch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept {
  // Create an IR step and add it to the output trace
  auto step = make_shared<Launch>(child);
  _trace->addTracedStep(c, step);

  // Add the child command to the trace
  _trace->addCommand(child);

  // Inform observers of the launch
  observeLaunch(c, child);

  // Show the command if printing is on, or if this is a dry run
  if (options::print_on_run) {
    cout << child->getShortName(options::command_length) << endl;
  }

  // The child command will be executed by this build.
  child->setExecuted();

  // The child command requires that its working directory exists
  child->getInitialWorkingDir()->getArtifact()->mustExist(*this, child);

  // The executable must be fully committed
  child->getExecutable()->getArtifact()->commitAll();

  // The child command also depends on the artifacts reachable through its initial FDs
  for (auto& [index, desc] : child->getInitialFDs()) {
    auto artifact = desc.getRef()->getArtifact();

    // TODO: Handle pipes eventually. Just skip them for now
    if (artifact->as<PipeArtifact>()) continue;

    if (artifact->canCommitAll()) {
      artifact->commitAll();
    } else {
      WARN << "Launching " << child << " without committing referenced artifact " << artifact;
    }
  }
}

// This command joined with a child command
void Build::traceJoin(shared_ptr<Command> c, shared_ptr<Command> child, int exit_status) noexcept {
  // Create an IR step and add it to the output trace
  auto step = make_shared<Join>(child, exit_status);
  _trace->addTracedStep(c, step);

  // Save the exit status in the child (TODO: Remove this once we know Build::exit works)
  child->setExitStatus(exit_status);
}

void Build::traceExit(shared_ptr<Command> c, int exit_status) noexcept {
  // Create an IR step and add it to the output trace
  auto step = make_shared<Exit>(exit_status);
  _trace->addTracedStep(c, step);

  // Record that the command has exited
  _exited.insert(c);

  // Save the exit status for this command (TODO: remove once EXIT changes are supported for real)
  c->setExitStatus(exit_status);
}
