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
#include "util/path.hh"
#include "versions/DirVersion.hh"
#include "versions/FileVersion.hh"
#include "versions/MetadataVersion.hh"
#include "versions/SymlinkVersion.hh"

using std::cout;
using std::endl;
using std::make_unique;
using std::ostream;
using std::shared_ptr;

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
void Build::observeCommandChange(shared_ptr<Command> c) const noexcept {
  for (const auto& o : _observers) o->commandChanged(c);
}

// Inform observers that the version of an artifact produced during the build does not match the
// on-disk version.
void Build::observeFinalMismatch(shared_ptr<Artifact> a,
                                 shared_ptr<Version> produced,
                                 shared_ptr<Version> ondisk) const noexcept {
  for (const auto& o : _observers) o->finalMismatch(a, produced, ondisk);
}

/************************ Handle IR steps from a loaded trace ************************/

void Build::finish() noexcept {
  // Wait for all remaining processes to exit
  _tracer.wait();

  // Compare the final state of all artifacts to the actual filesystem
  _env->getRootDir()->checkFinalState(*this, "/");

  /// Commit the final environment state to the filesystem
  if (_commit) _env->commitFinalState();

  // If there is an output trace, pass along the finished state
  if (_output_trace) _output_trace->finish();
}

void Build::specialRef(shared_ptr<Command> c,
                       SpecialRef::Entity entity,
                       shared_ptr<RefResult> output) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->specialRef(c, entity, output);
  }

  // Resolve the reference
  if (entity == SpecialRef::stdin) {
    output->resolvesTo(_env->getStdin(*this, c));

  } else if (entity == SpecialRef::stdout) {
    output->resolvesTo(_env->getStdout(*this, c));

  } else if (entity == SpecialRef::stderr) {
    output->resolvesTo(_env->getStderr(*this, c));

  } else if (entity == SpecialRef::root) {
    output->resolvesTo(_env->getRootDir());

  } else if (entity == SpecialRef::cwd) {
    auto cwd_path = fs::current_path().relative_path();
    auto result = _env->getRootDir()->resolve(*this, c, nullptr, cwd_path.begin(), cwd_path.end(),
                                              AccessFlags{.x = true}, output, false);
    ASSERT(result) << "Failed to resolve current working directory";
    result->setName(".");

    output->resolvesTo(result);

  } else if (entity == SpecialRef::launch_exe) {
    auto dodo = readlink("/proc/self/exe");
    auto dodo_launch = (dodo.parent_path() / "dodo-launch").relative_path();
    auto result =
        _env->getRootDir()->resolve(*this, c, nullptr, dodo_launch.begin(), dodo_launch.end(),
                                    AccessFlags{.x = true}, output, false);

    output->resolvesTo(result);

  } else {
    FAIL << "Unknown special reference";
  }
}

// A command references a new anonymous pipe
void Build::pipeRef(shared_ptr<Command> c,
                    shared_ptr<RefResult> read_end,
                    shared_ptr<RefResult> write_end) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->pipeRef(c, read_end, write_end);
  }

  // Resolve the reference and save the result in output
  auto pipe = _env->getPipe(*this, c);
  read_end->resolvesTo(pipe);
  write_end->resolvesTo(pipe);
}

// A command references a new anonymous file
void Build::fileRef(shared_ptr<Command> c, mode_t mode, shared_ptr<RefResult> output) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->fileRef(c, mode, output);
  }

  // Resolve the reference and save the result in output
  output->resolvesTo(_env->createFile(*this, c, mode, false));
}

// A command references a new anonymous symlink
void Build::symlinkRef(shared_ptr<Command> c,
                       fs::path target,
                       shared_ptr<RefResult> output) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->symlinkRef(c, target, output);
  }

  // Resolve the reference and save the result in output
  output->resolvesTo(_env->getSymlink(*this, c, target, false));
}

// A command references a new anonymous directory
void Build::dirRef(shared_ptr<Command> c, mode_t mode, shared_ptr<RefResult> output) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->dirRef(c, mode, output);
  }

  // Resolve the reference and save the result in output
  output->resolvesTo(_env->getDir(*this, c, mode, false));
}

// A command makes a reference with a path
void Build::pathRef(shared_ptr<Command> c,
                    shared_ptr<RefResult> base,
                    fs::path path,
                    AccessFlags flags,
                    shared_ptr<RefResult> output) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->pathRef(c, base, path, flags, output);
  }

  // Resolve the reference and save the result in output
  ASSERT(base->getResult()) << "Cannot resolve a path relative to an unresolved base reference.";
  auto result =
      base->getResult()->resolve(*this, c, nullptr, path.begin(), path.end(), flags, output, false);
  output->resolvesTo(result);
}

// Command c expects a reference to resolve with a specific result
void Build::expectResult(shared_ptr<Command> c, shared_ptr<RefResult> ref, int expected) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->expectResult(c, ref, expected);
  }

  // Does the resolved reference match the expected result?
  if (ref->getResult() != expected) {
    observeCommandChange(c);
  }
}

// Command c accesses an artifact's metadata
void Build::matchMetadata(shared_ptr<Command> c,
                          shared_ptr<RefResult> ref,
                          shared_ptr<MetadataVersion> expected) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->matchMetadata(c, ref, expected);
  }

  // If the reference is not resolved, a change must have occurred
  if (!ref->getResult()) {
    // Report the change and return
    observeCommandChange(c);
    return;
  }

  // Perform the comparison
  ref->getResult()->matchMetadata(*this, c, expected);
}

// Command c accesses an artifact's content
void Build::matchContent(shared_ptr<Command> c,
                         shared_ptr<RefResult> ref,
                         shared_ptr<Version> expected) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->matchContent(c, ref, expected);
  }

  // If the reference is not resolved, a change must have occurred
  if (!ref->getResult()) {
    // Report the change and return
    observeCommandChange(c);
    return;
  }

  // Perform the comparison
  ref->getResult()->matchContent(*this, c, expected);
}

// Command c modifies an artifact
void Build::updateMetadata(shared_ptr<Command> c,
                           shared_ptr<RefResult> ref,
                           shared_ptr<MetadataVersion> written) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->updateMetadata(c, ref, written);
  }

  // If the reference is not resolved, a change must have occurred
  if (!ref->getResult()) {
    // Record the change and return
    observeCommandChange(c);
    return;
  }

  // Make sure this version is NOT marked as committed
  written->setCommitted(false);

  // Mark the version as created by the calling command. This field is transient, so we have to
  // apply it on ever run
  written->createdBy(c);

  // Apply the write
  ref->getResult()->updateMetadata(*this, c, written);
}

// Command c modifies an artifact
void Build::updateContent(shared_ptr<Command> c,
                          shared_ptr<RefResult> ref,
                          shared_ptr<Version> written) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->updateContent(c, ref, written);
  }

  // If the reference is not resolved, a change must have occurred
  if (!ref->getResult()) {
    // Record the change and return
    observeCommandChange(c);
    return;
  }

  // Make sure this version is NOT marked as committed
  written->setCommitted(false);

  // Mark the version as created by the calling command. This field is transient, so we have to
  // apply it on ever run
  written->createdBy(c);

  // Apply the write
  written->applyTo(*this, c, ref->getResult());

  // Save the last write
  _last_write = {c, ref, written};
}

// This command launches a child command
void Build::launch(shared_ptr<Command> c, shared_ptr<Command> child) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

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
    child->getInitialWorkingDir()->getResult()->mustExist(*this, child);

    // The executable must be fully committed
    child->getExecutable()->getResult()->commitAll();

    // The child command also depends on the artifacts reachable through its initial FDs
    for (auto& [index, desc] : child->getInitialFDs()) {
      auto artifact = desc.getRef()->getResult();

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

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->launch(c, child);
  }
}

// This command joined with a child command
void Build::join(shared_ptr<Command> c, shared_ptr<Command> child, int exit_status) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->join(c, child, exit_status);
  }

  // If the child command is running in the tracer, wait for it
  if (isRunning(child)) _tracer.wait(_running[child]);

  // Did the child command's exit status match the expected result?
  if (child->getExitStatus() != exit_status) {
    observeCommandChange(c);
  }
}

void Build::exit(shared_ptr<Command> c, int exit_status) noexcept {
  // If this step comes from a command we cannot emulate, skip it
  if (!_plan.canEmulate(c)) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->exit(c, exit_status);
  }

  // Record that the command has exited
  _exited.insert(c);

  // Save the exit status for this command (TODO: remove once EXIT changes are supported for real)
  c->setExitStatus(exit_status);
}

/************************ Trace IR Steps ************************/

// A command references a new anonymous pipe
tuple<shared_ptr<RefResult>, shared_ptr<RefResult>> Build::tracePipeRef(
    shared_ptr<Command> c) noexcept {
  // Create RefResults to hold the two ends of the pipe
  auto read_end = make_shared<RefResult>();
  auto write_end = make_shared<RefResult>();

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->pipeRef(c, read_end, write_end);
  }

  // Resolve the reference and save the result in output
  auto pipe = _env->getPipe(*this, c);
  read_end->resolvesTo(pipe);
  write_end->resolvesTo(pipe);

  return {read_end, write_end};
}

// A command references a new anonymous file
shared_ptr<RefResult> Build::traceFileRef(shared_ptr<Command> c, mode_t mode) noexcept {
  // Create a RefResult to hold the result of the resolution
  auto output = make_shared<RefResult>();

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->fileRef(c, mode, output);
  }

  // Resolve the reference and save the result in output
  output->resolvesTo(_env->createFile(*this, c, mode, true));

  return output;
}

// A command references a new anonymous symlink
shared_ptr<RefResult> Build::traceSymlinkRef(shared_ptr<Command> c, fs::path target) noexcept {
  // Create a RefResult to hold the result of the resolution
  auto output = make_shared<RefResult>();

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->symlinkRef(c, target, output);
  }

  // Resolve the reference and save the result in output
  output->resolvesTo(_env->getSymlink(*this, c, target, true));

  return output;
}

// A command references a new anonymous directory
shared_ptr<RefResult> Build::traceDirRef(shared_ptr<Command> c, mode_t mode) noexcept {
  // Create a RefResult to hold the result of the resolution
  auto output = make_shared<RefResult>();

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->dirRef(c, mode, output);
  }

  // Resolve the reference and save the result in output
  output->resolvesTo(_env->getDir(*this, c, mode, true));

  return output;
}

// A command makes a reference with a path
shared_ptr<RefResult> Build::tracePathRef(shared_ptr<Command> c,
                                          shared_ptr<RefResult> base,
                                          fs::path path,
                                          AccessFlags flags) noexcept {
  // Create a RefResult to hold the result of the resolution
  auto output = make_shared<RefResult>();

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->pathRef(c, base, path, flags, output);
  }

  // Resolve the reference and save the result in output
  ASSERT(base->getResult()) << "Cannot resolve a path relative to an unresolved base reference.";
  auto result =
      base->getResult()->resolve(*this, c, nullptr, path.begin(), path.end(), flags, output, true);
  output->resolvesTo(result);

  return output;
}

// Command c expects a reference to resolve with a specific result
void Build::traceExpectResult(shared_ptr<Command> c,
                              shared_ptr<RefResult> ref,
                              int expected) noexcept {
  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->expectResult(c, ref, expected);
  }
}

// Command c accesses an artifact's metadata
void Build::traceMatchMetadata(shared_ptr<Command> c, shared_ptr<RefResult> ref) noexcept {
  // Get the artifact whose metadata is being accessed
  auto artifact = ref->getResult();
  ASSERT(artifact) << "Tried to access metadata through unresolved reference " << ref;

  // Get the current metadata from the artifact
  auto expected = artifact->getMetadata(*this, c, InputType::Accessed);
  ASSERT(expected) << "Unable to get metadata from " << artifact;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->matchMetadata(c, ref, expected);
  }

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
void Build::traceMatchContent(shared_ptr<Command> c, shared_ptr<RefResult> ref) noexcept {
  // Get the artifact whose content is being accessed
  auto artifact = ref->getResult();
  ASSERT(artifact) << "Tried to access content through an unresolved reference " << ref;

  // Get the current content of the artifact
  auto expected = artifact->getContent(*this, c, InputType::Accessed);
  ASSERT(expected) << "Unable to get content from " << artifact;

  // If this access is from the same command and reference as the last write, and the versions are
  // the same, skip the trace step
  if (_last_write == tuple{c, ref, expected}) return;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->matchContent(c, ref, expected);
  }

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
void Build::traceUpdateMetadata(shared_ptr<Command> c, shared_ptr<RefResult> ref) noexcept {
  // Get the artifact whose metadata is being written
  auto artifact = ref->getResult();
  ASSERT(artifact) << "Tried to write metadata through an unresolved reference " << ref;

  // Record the update and get the written version
  auto written = artifact->updateMetadata(*this, c);
  ASSERT(written) << "Unable to get written metadata version from " << artifact;

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->updateMetadata(c, ref, written);
  }

  // The calling command created this version
  written->createdBy(c);

  // This apply operation was traced, so the written version is committed
  written->setCommitted();
}

// Command c modifies an artifact
void Build::traceUpdateContent(shared_ptr<Command> c,
                               shared_ptr<RefResult> ref,
                               shared_ptr<Version> written) noexcept {
  // Get the artifact whose content is being written
  auto artifact = ref->getResult();
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
  if (_output_trace) {
    _output_trace->updateContent(c, ref, written);
  }

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
  // The child command will be executed by this build.
  child->setExecuted();

  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->launch(c, child);
  }

  // Inform observers of the launch
  observeLaunch(c, child);

  // Show the command if printing is on, or if this is a dry run
  if (options::print_on_run) {
    cout << child->getShortName(options::command_length) << endl;
  }

  // The child command requires that its working directory exists
  child->getInitialWorkingDir()->getResult()->mustExist(*this, child);

  // The executable must be fully committed
  child->getExecutable()->getResult()->commitAll();

  // The child command also depends on the artifacts reachable through its initial FDs
  for (auto& [index, desc] : child->getInitialFDs()) {
    auto artifact = desc.getRef()->getResult();

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
  if (_output_trace) {
    _output_trace->join(c, child, exit_status);
  }

  // Save the exit status in the child (TODO: Remove this once we know Build::exit works)
  child->setExitStatus(exit_status);
}

void Build::traceExit(shared_ptr<Command> c, int exit_status) noexcept {
  // Create an IR step and add it to the output trace
  if (_output_trace) {
    _output_trace->exit(c, exit_status);
  }

  // Record that the command has exited
  _exited.insert(c);

  // Save the exit status for this command (TODO: remove once EXIT changes are supported for real)
  c->setExitStatus(exit_status);
}
