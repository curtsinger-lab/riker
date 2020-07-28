#include "Build.hh"

#include <iostream>
#include <memory>
#include <ostream>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "artifacts/SymlinkArtifact.hh"
#include "build/Env.hh"
#include "build/Resolution.hh"
#include "core/IR.hh"
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

tuple<shared_ptr<Trace>, shared_ptr<Env>> Build::run(shared_ptr<Trace> trace,
                                                     shared_ptr<Env> env) noexcept {
  // If no environment was provided, use a default environment
  if (!env) env = make_shared<Env>();

  // Set the current environment
  _env = env;

  // The current trace is a restarted version of the provided trace
  // It has no steps or commands, but retains its initial references
  _trace = trace->restart();

  // Resolve all the initial references in the trace (root, cwd, stdin, stdout, etc.)
  _trace->resolveRefs(*this, _env);

  // Walk through the steps in the provided trace
  for (auto& [cmd, step] : trace->getSteps()) {
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

  // Compare the final state of all artifacts to the actual filesystem
  _env->getRootDir()->checkFinalState(*this, "/");

  return {_trace, _env};
}

/************************ Command Tracing and Emulation ************************/

// Command c creates a new pipe
shared_ptr<Pipe> Build::pipe(shared_ptr<Command> c, shared_ptr<Pipe> emulating) noexcept {
  auto ref = emulating;
  if (!emulating) ref = make_shared<Pipe>();
  ref->resolvesTo(_env->getPipe(*this, c));
  _trace->addStep(c, ref);
  return ref;
}

// Command c creates a new symbolic link
shared_ptr<Symlink> Build::symlink(shared_ptr<Command> c,
                                   fs::path target,
                                   shared_ptr<Symlink> emulating) noexcept {
  auto ref = emulating;
  if (!emulating) ref = make_shared<Symlink>(target);
  ref->resolvesTo(_env->getSymlink(*this, c, target, !emulating));
  _trace->addStep(c, ref);
  return ref;
}

// Command c creates a new directory
shared_ptr<Dir> Build::dir(shared_ptr<Command> c, mode_t mode, shared_ptr<Dir> emulating) noexcept {
  auto ref = emulating;
  if (!emulating) ref = make_shared<Dir>(mode);
  ref->resolvesTo(_env->getDir(*this, c, mode, !emulating));
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
  ref->resolve(*this, c, !emulating);

  // If the access is being emulated, check the result
  if (emulating && ref->getResolution() != ref->getExpectedResult()) {
    observeCommandChange(c, emulating);
  }

  // Add the reference to the new build trace
  _trace->addStep(c, ref);

  return ref;
}

// Command c accesses an artifact
template <class VersionType>
void Build::match(shared_ptr<Command> c,
                  shared_ptr<Ref> ref,
                  shared_ptr<VersionType> expected,
                  shared_ptr<Match<VersionType>> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << "A traced command read through an unresolved reference";

    // Report the change
    observeCommandChange(c, emulating);

    // Add the step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating);
    return;
  }

  // Are we emulating this operation?
  if (emulating) {
    // Yes. We need an expected version to check for
    ASSERT(expected) << "Traced command provided an expected version to match";

    // Perform the comparison
    ref->getArtifact()->match(*this, c, expected);

    // Record the emulated trace step
    _trace->addStep(c, emulating);

  } else {
    // No. This is a traced command

    // If we don't have an expected version already, get one from the current state
    if (!expected) expected = ref->getArtifact()->get<VersionType>(*this, c, InputType::Accessed);

    ASSERT(expected) << "Unable to get expected version from artifact " << ref->getArtifact();

    // If a different command created this version, fingerprint it for later comparison
    auto creator = expected->getCreator();
    if (!creator || creator != c) {
      // We can only take a fingerprint with a path
      if (auto access = ref->as<Access>()) expected->fingerprint(access->getFullPath());
    }

    // Add a match step to the trace
    _trace->addStep(c, make_shared<Match<VersionType>>(ref, expected));
  }
}

// Explicitly instantiate match for metadata
template void Build::match<MetadataVersion>(shared_ptr<Command> c,
                                            shared_ptr<Ref> ref,
                                            shared_ptr<MetadataVersion> expected,
                                            shared_ptr<Match<MetadataVersion>> emulating) noexcept;

// Explicitly instantiate match for content
template void Build::match<FileVersion>(shared_ptr<Command> c,
                                        shared_ptr<Ref> ref,
                                        shared_ptr<FileVersion> expected,
                                        shared_ptr<Match<FileVersion>> emulating) noexcept;

// Explicitly instantiate match for symlinks
template void Build::match<SymlinkVersion>(shared_ptr<Command> c,
                                           shared_ptr<Ref> ref,
                                           shared_ptr<SymlinkVersion> expected,
                                           shared_ptr<Match<SymlinkVersion>> emulating) noexcept;

// Explicitly instantiate match for directory lists
template void Build::match<ListedDir>(shared_ptr<Command> c,
                                      shared_ptr<Ref> ref,
                                      shared_ptr<ListedDir> expected,
                                      shared_ptr<Match<ListedDir>> emulating) noexcept;

// Command c modifies an artifact
template <class VersionType>
void Build::apply(shared_ptr<Command> c,
                  shared_ptr<Ref> ref,
                  shared_ptr<VersionType> written,
                  shared_ptr<Apply<VersionType>> emulating) noexcept {
  // If the reference is not resolved, a change must have occurred
  if (!ref->isResolved()) {
    ASSERT(emulating) << "A traced command tried to write through an unresolved reference";

    // Record the change
    observeCommandChange(c, emulating);

    // Add the IR step and return. Nothing else to do, since there's no artifact
    _trace->addStep(c, emulating);
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
    ref->getArtifact()->apply(*this, c, written);

    // Add this write to the trace
    _trace->addStep(c, emulating);

  } else {
    // No. This is a traced operation

    // The caller has to provide us with a valid version to apply
    ASSERT(written) << "apply called with no written version";

    // The calling command created this version
    written->createdBy(c);

    // This apply operation was traced, so the written version is committed
    written->setCommitted();

    // Apply the write, which is committed to the filesystem because we just traced this operation
    ref->getArtifact()->apply(*this, c, written);

    // Add a new trace step
    _trace->addStep(c, make_shared<Apply<VersionType>>(ref, written));
  }
}

// Explicitly instantiate apply for metadata versions
template void Build::apply<MetadataVersion>(shared_ptr<Command> c,
                                            shared_ptr<Ref> ref,
                                            shared_ptr<MetadataVersion> written,
                                            shared_ptr<Apply<MetadataVersion>> emulating) noexcept;

// Explicitly instantiate apply for content versions
template void Build::apply<FileVersion>(shared_ptr<Command> c,
                                        shared_ptr<Ref> ref,
                                        shared_ptr<FileVersion> written,
                                        shared_ptr<Apply<FileVersion>> emulating) noexcept;

// Explicitly instantiate apply for directory link versions
template void Build::apply<AddEntry>(shared_ptr<Command> c,
                                     shared_ptr<Ref> ref,
                                     shared_ptr<AddEntry> written,
                                     shared_ptr<Apply<AddEntry>> emulating) noexcept;

// Explicitly instantiate apply for directory unlink versions
template void Build::apply<RemoveEntry>(shared_ptr<Command> c,
                                        shared_ptr<Ref> ref,
                                        shared_ptr<RemoveEntry> written,
                                        shared_ptr<Apply<RemoveEntry>> emulating) noexcept;

// This command launches a child command
void Build::launch(shared_ptr<Command> c,
                   shared_ptr<Command> child,
                   shared_ptr<Launch> emulating) noexcept {
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
    if (options::print_on_run || options::dry_run) {
      cout << child->getShortName(options::command_length) << endl;
    }

    // Is this a real execution and not a dry run?
    if (!options::dry_run) {
      // Yes. The child command will be executed by this build.
      child->setExecuted();

      // The child command depends on all the references it inherits as file descriptors
      for (auto& [index, desc] : child->getInitialFDs()) {
        if (auto access = desc.getRef()->as<Access>()) {
          WARN << "Resolving " << access->getRelativePath();
          access->resolve(*this, child, true);
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

  // Add the launch step to the trace
  _trace->addStep(c, step);
}

// This command joined with a child command
void Build::join(shared_ptr<Command> c,
                 shared_ptr<Command> child,
                 int exit_status,
                 shared_ptr<Join> emulating) noexcept {
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
