#include "Build.hh"

#include <cstdio>
#include <filesystem>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "artifacts/SymlinkArtifact.hh"
#include "data/AccessFlags.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "runtime/env.hh"
#include "runtime/policy.hh"
#include "tracing/Tracer.hh"
#include "ui/TracePrinter.hh"
#include "ui/options.hh"
#include "ui/stats.hh"
#include "util/log.hh"
#include "util/wrappers.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirVersion.hh"
#include "versions/MetadataVersion.hh"

using std::cout;
using std::endl;
using std::list;
using std::make_shared;
using std::map;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

namespace fs = std::filesystem;

// Can a step from the provided command be emulated?
bool Build::canEmulate(const shared_ptr<Command>& c) noexcept {
  return !_execute || !c->running();
}

/************************ Handle IR steps from a loaded trace ************************/

void Build::finish() noexcept {
  // Wait for all remaining processes to exit
  _tracer.wait();

  // Compare the final state of all artifacts to the actual filesystem
  env::getRootDir()->checkFinalState("/");

  // Mark all commands as finished
  for (auto& c : _commands) {
    LOG(exec) << "Finishing " << c;
    c->finishRun();
  }

  // Inform the output trace that it is finished
  _output.finish();

  // Plan the next build
  for (auto& c : _commands) {
    c->planBuild();
  }
}

void Build::specialRef(const shared_ptr<Command>& c, SpecialRef entity, Ref::ID output) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::SpecialRefPrinter{c, entity, output};

  // Create an IR step and add it to the output trace
  _output.specialRef(c, entity, output);

  // Resolve the reference
  if (entity == SpecialRef::stdin) {
    // Create the stdin ref. Add one user, which accounts for the build tool itself
    // That way we won't close stdin when the build is finishing
    auto stdin_ref = make_shared<Ref>(ReadAccess, env::getStdin(c));
    stdin_ref->addUser();
    c->currentRun()->setRef(output, stdin_ref);

  } else if (entity == SpecialRef::stdout) {
    // Create the stdout ref and add one user (the build tool)
    auto stdout_ref = make_shared<Ref>(WriteAccess, env::getStdout(c));
    stdout_ref->addUser();
    c->currentRun()->setRef(output, stdout_ref);

  } else if (entity == SpecialRef::stderr) {
    // Create the stderr ref and add one user (the build tool)
    auto stderr_ref = make_shared<Ref>(WriteAccess, env::getStderr(c));
    stderr_ref->addUser();
    c->currentRun()->setRef(output, stderr_ref);

  } else if (entity == SpecialRef::root) {
    c->currentRun()->setRef(output, make_shared<Ref>(ReadAccess + ExecAccess, env::getRootDir()));

  } else if (entity == SpecialRef::cwd) {
    auto cwd_path = fs::current_path().relative_path();
    auto ref = make_shared<Ref>(env::getRootDir()->resolve(c, cwd_path, ReadAccess + ExecAccess));
    c->currentRun()->setRef(output, ref);

    ASSERT(ref->isSuccess()) << "Failed to resolve current working directory";
    ref->getArtifact()->setName(".");

  } else if (entity == SpecialRef::launch_exe) {
    auto dodo = readlink("/proc/self/exe");
    auto dodo_launch = (dodo.parent_path() / "dodo-launch").relative_path();

    auto ref =
        make_shared<Ref>(env::getRootDir()->resolve(c, dodo_launch, ReadAccess + ExecAccess));
    c->currentRun()->setRef(output, ref);

  } else {
    FAIL << "Unknown special reference";
  }
}

// A command references a new anonymous pipe
void Build::pipeRef(const shared_ptr<Command>& c, Ref::ID read_end, Ref::ID write_end) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::PipeRefPrinter{c, read_end, write_end};

  // Create an IR step and add it to the output trace
  _output.pipeRef(c, read_end, write_end);

  // Resolve the reference and save the result in output
  auto pipe = env::getPipe(c);
  c->currentRun()->setRef(read_end, make_shared<Ref>(ReadAccess, pipe));
  c->currentRun()->setRef(write_end, make_shared<Ref>(WriteAccess, pipe));
}

// A command references a new anonymous file
void Build::fileRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::FileRefPrinter{c, mode, output};

  // Create an IR step and add it to the output trace
  _output.fileRef(c, mode, output);

  // Resolve the reference and save the result in output
  c->currentRun()->setRef(output,
                          make_shared<Ref>(ReadAccess + WriteAccess, env::createFile(c, mode)));
}

// A command references a new anonymous symlink
void Build::symlinkRef(const shared_ptr<Command>& c, fs::path target, Ref::ID output) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::SymlinkRefPrinter{c, target, output};

  // Create an IR step and add it to the output trace
  _output.symlinkRef(c, target, output);

  // Resolve the reference and save the result in output
  c->currentRun()->setRef(
      output, make_shared<Ref>(ReadAccess + WriteAccess + ExecAccess, env::getSymlink(c, target)));
}

// A command references a new anonymous directory
void Build::dirRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::DirRefPrinter{c, mode, output};

  // Create an IR step and add it to the output trace
  _output.dirRef(c, mode, output);

  // Resolve the reference and save the result in output
  c->currentRun()->setRef(
      output, make_shared<Ref>(ReadAccess + WriteAccess + ExecAccess, env::getDir(c, mode)));
}

// A command makes a reference with a path
void Build::pathRef(const shared_ptr<Command>& c,
                    Ref::ID base,
                    fs::path path,
                    AccessFlags flags,
                    Ref::ID output) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::PathRefPrinter{c, base, path, flags, output};

  // Create an IR step and add it to the output trace
  _output.pathRef(c, base, path, flags, output);

  // Get the directory where resolution should begin
  auto base_dir = c->currentRun()->getRef(base)->getArtifact();

  // Resolve the reference and save the result in output
  ASSERT(base_dir) << "Cannot resolve a path relative to an unresolved base reference.";

  c->currentRun()->setRef(output, make_shared<Ref>(base_dir->resolve(c, path, flags)));
}

// A command retains a handle to a given Ref
void Build::usingRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;
  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::UsingRefPrinter{c, ref};

  // Command c is now using ref
  c->currentRun()->usingRef(ref);

  // Create an IR step and add it to the output trace
  _output.usingRef(c, ref);
}

// A command closes a handle to a given Ref
void Build::doneWithRef(const shared_ptr<Command>& c, Ref::ID ref_id) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Command c is no longer using ref
  c->currentRun()->doneWithRef(ref_id);

  // If this is the final use of the ref, inform the referenced artifact of the close
  auto ref = c->currentRun()->getRef(ref_id);
  if (ref->getUserCount() == 0) {
    auto a = ref->getArtifact();
    if (a) {
      LOG(exec) << c << " closing final ref to " << a << " with flags " << ref->getFlags();
      a->beforeClose(*this, c, ref_id);
    }
  }

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::DoneWithRefPrinter{c, ref_id};

  // Create an IR step and add it to the output trace
  _output.doneWithRef(c, ref_id);
}

// Command c depends on the outcome of comparing two different references
void Build::compareRefs(const shared_ptr<Command>& c,
                        Ref::ID ref1_id,
                        Ref::ID ref2_id,
                        RefComparison type) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::CompareRefsPrinter{c, ref1_id, ref2_id, type};

  // Create an IR step and add it to the output trace
  _output.compareRefs(c, ref1_id, ref2_id, type);

  auto ref1 = c->currentRun()->getRef(ref1_id);
  auto ref2 = c->currentRun()->getRef(ref2_id);

  // Does the comparison resolve as expected?
  if (type == RefComparison::SameInstance) {
    if (ref1->getArtifact() != ref2->getArtifact()) {
      LOGF(rebuild, "{} changed: expected {} and {} to refer to the same artifact", c, ref1, ref2);

      c->currentRun()->observeChange(Scenario::Build);
      c->currentRun()->observeChange(Scenario::PostBuild);
    }
  } else if (type == RefComparison::DifferentInstances) {
    if (ref1->getArtifact() == ref2->getArtifact()) {
      LOGF(rebuild, "{} changed: expected {} and {} to refer to different artifacts", c, ref1,
           ref2);

      c->currentRun()->observeChange(Scenario::Build);
      c->currentRun()->observeChange(Scenario::PostBuild);
    }
  } else {
    FAIL << "Unknown reference comparison type";
  }
}

// Command c expects a reference to resolve with a specific result
void Build::expectResult(const shared_ptr<Command>& c,
                         Scenario scenario,
                         Ref::ID ref_id,
                         int expected) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::ExpectResultPrinter{c, scenario, ref_id, expected};

  // Create an IR step and add it to the output trace
  _output.expectResult(c, scenario, ref_id, expected);

  // Does the resolved reference match the expected result?
  auto ref = c->currentRun()->getRef(ref_id);
  if (ref->getResultCode() != expected) {
    LOGF(rebuild,
         "{} changed in scenario {}: {} did not resolve as expected (expected {}, observed {})", c,
         scenario, ref, expected, ref->getResultCode());
    c->currentRun()->observeChange(scenario);
  }
}

// Command c accesses an artifact's metadata
void Build::matchMetadata(const shared_ptr<Command>& c,
                          Scenario scenario,
                          Ref::ID ref_id,
                          shared_ptr<MetadataVersion> expected) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::MatchMetadataPrinter{c, scenario, ref_id, expected};

  // Create an IR step and add it to the output trace
  _output.matchMetadata(c, scenario, ref_id, expected);

  auto ref = c->currentRun()->getRef(ref_id);

  // We can't do anything with an unresolved reference. A change should already have been reported.
  if (!ref->isResolved()) return;

  // Perform the comparison
  ref->getArtifact()->matchMetadata(c, scenario, expected);
}

// Command c accesses an artifact's content
void Build::matchContent(const shared_ptr<Command>& c,
                         Scenario scenario,
                         Ref::ID ref_id,
                         shared_ptr<ContentVersion> expected) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::MatchContentPrinter{c, scenario, ref_id, expected};

  // Create an IR step and add it to the output trace
  _output.matchContent(c, scenario, ref_id, expected);

  auto ref = c->currentRun()->getRef(ref_id);

  // We can't do anything with an unresolved reference. A change should already have been reported.
  if (!ref->isResolved()) return;

  // Perform the comparison
  ref->getArtifact()->matchContent(c, scenario, expected);
}

// Command c modifies an artifact
void Build::updateMetadata(const shared_ptr<Command>& c,
                           Ref::ID ref_id,
                           shared_ptr<MetadataVersion> written) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::UpdateMetadataPrinter{c, ref_id, written};

  // Create an IR step and add it to the output trace
  _output.updateMetadata(c, ref_id, written);

  auto ref = c->currentRun()->getRef(ref_id);

  // We can't do anything with an unresolved reference. A change should already have been reported.
  if (!ref->isResolved()) return;

  // Apply the write
  ref->getArtifact()->updateMetadata(c, written);
}

// Command c modifies an artifact
void Build::updateContent(const shared_ptr<Command>& c,
                          Ref::ID ref_id,
                          shared_ptr<ContentVersion> written) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::UpdateContentPrinter{c, ref_id, written};

  // Create an IR step and add it to the output trace
  _output.updateContent(c, ref_id, written);

  auto ref = c->currentRun()->getRef(ref_id);

  // We can't do anything with an unresolved reference. A change should already have been reported.
  if (!ref->isResolved()) return;

  // Mark the version as created by the calling command. This field is transient, so we have to
  // apply it on ever run
  written->createdBy(c);

  // Apply the write
  ref->getArtifact()->updateContent(c, written);
}

/// Handle an AddEntry IR step
void Build::addEntry(const shared_ptr<Command>& c,
                     Ref::ID dir_id,
                     fs::path name,
                     Ref::ID target_id) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::AddEntryPrinter{c, dir_id, name, target_id};

  // Create an IR step and add it to the output trace
  _output.addEntry(c, dir_id, name, target_id);

  auto dir = c->currentRun()->getRef(dir_id);
  auto target = c->currentRun()->getRef(target_id);

  // We can't do anything with unresolved references. A change should already have been reported.
  if (!dir->isResolved() || !target->isResolved()) return;

  // Add the entry to the directory
  dir->getArtifact()->addEntry(c, name, target->getArtifact());
}

/// Handle a RemoveEntry IR step
void Build::removeEntry(const shared_ptr<Command>& c,
                        Ref::ID dir_id,
                        fs::path name,
                        Ref::ID target_id) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::RemoveEntryPrinter{c, dir_id, name, target_id};

  // Create an IR step and add it to the output trace
  _output.removeEntry(c, dir_id, name, target_id);

  auto dir = c->currentRun()->getRef(dir_id);
  auto target = c->currentRun()->getRef(target_id);

  // We can't do anything with unresolved references. A change should already have been reported.
  if (!dir->isResolved() || !target->isResolved()) return;

  // Remove the entry from the directory
  dir->getArtifact()->removeEntry(c, name, target->getArtifact());
}

// This command launches a child command
void Build::launch(const shared_ptr<Command>& c,
                   const shared_ptr<Command>& child,
                   list<tuple<Ref::ID, Ref::ID>> refs) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::LaunchPrinter{c, child, refs};

  // Assign references in the child command
  for (const auto& [parent_ref_id, child_ref_id] : refs) {
    child->currentRun()->setRef(child_ref_id, c->currentRun()->getRef(parent_ref_id));
  }

  // If we're emulating the launch of an unexecuted command, report the change to the command
  if (!child->hasExecuted()) {
    LOGF(rebuild, "{} changed: never run", child);
    child->currentRun()->observeChange(Scenario::Build);
    child->currentRun()->observeChange(Scenario::PostBuild);
  }

  // Remember that this command was run by the build
  _commands.insert(child);

  // Add the child to the parent command's set of children
  c->currentRun()->addChild(child);

  // Are we going to re-execute the child?
  bool launch_command = false;

  // Should we print the child command?
  bool print_command = false;

  if (!canEmulate(child)) {
    // Print the command if requested, or if this is a dry run
    if (options::print_on_run || options::dry_run) print_command = true;

    // Launch the command if this is not a dry run
    if (!options::dry_run) launch_command = true;
  }

  // Print the command if requested
  if (print_command) {
    cout << child->getShortName(options::command_length) << endl;
  }

  // If we're going to launch the command, mark it as executed now
  if (launch_command) child->setExecuted();

  // Now emit the launch IR step. This has to happen after updating the executed state of the
  // command (above) and before actually launching the command.
  _output.launch(c, child, refs);

  // Launch the command if requested
  if (launch_command) {
    // Count the traced command
    stats::traced_commands++;

    // Prepare the child command to execute by committing the necessary state from its references
    child->currentRun()->createLaunchDependencies(*this);

    LOG(exec) << c << " launching " << child;

    // Start the child command in the tracer
    _running[child] = _tracer.start(child);

  } else {
    // Count the emulated command
    stats::emulated_commands++;
  }
}

// This command joined with a child command
void Build::join(const shared_ptr<Command>& c,
                 const shared_ptr<Command>& child,
                 int exit_status) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // If the child command is running in the tracer, wait for it
  if (isRunning(child)) _tracer.wait(_running[child]);

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::JoinPrinter{c, child, exit_status};

  // Create an IR step and add it to the output trace
  _output.join(c, child, exit_status);

  // Did the child command's exit status match the expected result?
  if (child->currentRun()->getExitStatus() != exit_status) {
    LOGF(rebuild, "{} changed: child {} exited with different status (expected {}, observed {})", c,
         child, exit_status, child->currentRun()->getExitStatus());

    // TODO: Re-enable this once we have skipping
    // c->currentRun()->observeChange(Scenario::Build);
    // c->currentRun()->observeChange(Scenario::PostBuild);
    WARN << c << " should rerun because child " << child << " changed exit status.";
  }
}

void Build::exit(const shared_ptr<Command>& c, int exit_status) noexcept {
  // If we can't emulate this command, skip its IR step
  if (!canEmulate(c)) return;

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::ExitPrinter{c, exit_status};

  // Create an IR step and add it to the output trace
  _output.exit(c, exit_status);

  // Save the exit status for this command
  c->currentRun()->setExitStatus(exit_status);
}

/************************ Trace IR Steps ************************/

// A command references a new anonymous pipe
tuple<Ref::ID, Ref::ID> Build::tracePipeRef(const shared_ptr<Command>& c) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // Create a pipe artifact
  auto pipe = env::getPipe(c);

  // Set up references for the read and write ends of the pipe
  auto read_end = c->currentRun()->setRef(make_shared<Ref>(ReadAccess, pipe));
  auto write_end = c->currentRun()->setRef(make_shared<Ref>(WriteAccess, pipe));

  // Create an IR step and add it to the output trace
  _output.pipeRef(c, read_end, write_end);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::PipeRefPrinter{c, read_end, write_end};

  return {read_end, write_end};
}

// A command references a new anonymous file
Ref::ID Build::traceFileRef(const shared_ptr<Command>& c, mode_t mode) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // Create an anonymous file
  auto file = env::createFile(c, mode);

  // Create a reference for the new file
  auto output = c->currentRun()->setRef(make_shared<Ref>(ReadAccess + WriteAccess, file));

  // Create an IR step and add it to the output trace
  _output.fileRef(c, mode, output);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::FileRefPrinter{c, mode, output};

  return output;
}

// A command references a new anonymous symlink
Ref::ID Build::traceSymlinkRef(const shared_ptr<Command>& c, fs::path target) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // Create a symlink artifact
  auto symlink = env::getSymlink(c, target);

  // Create a reference to the new symlink
  auto output =
      c->currentRun()->setRef(make_shared<Ref>(ReadAccess + WriteAccess + ExecAccess, symlink));

  // Create an IR step and add it to the output trace
  _output.symlinkRef(c, target, output);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::SymlinkRefPrinter{c, target, output};

  return output;
}

// A command references a new anonymous directory
Ref::ID Build::traceDirRef(const shared_ptr<Command>& c, mode_t mode) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // Create a directory artifact
  auto dir = env::getDir(c, mode);

  // Create a reference to the new directory
  auto output =
      c->currentRun()->setRef(make_shared<Ref>(ReadAccess + WriteAccess + ExecAccess, dir));

  // Create an IR step and add it to the output trace
  _output.dirRef(c, mode, output);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::DirRefPrinter{c, mode, output};

  return output;
}

// A command makes a reference with a path
Ref::ID Build::tracePathRef(const shared_ptr<Command>& c,
                            Ref::ID base_id,
                            fs::path path,
                            AccessFlags flags) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // Get the base directory artifact
  auto base = c->currentRun()->getRef(base_id);
  ASSERT(base->isResolved()) << "Cannot resolve a path relative to an unresolved base reference.";

  // Resolve the path and create a Ref
  auto ref = make_shared<Ref>(base->getArtifact()->resolve(c, path, flags));

  // Add the refrence to the command
  auto output = c->currentRun()->setRef(ref);

  // Create an IR step and add it to the output trace
  _output.pathRef(c, base_id, path, flags, output);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::PathRefPrinter{c, base_id, path, flags, output} << " -> "
          << c->currentRun()->getRef(output);

  return output;
}

// A command kept a handle to a Ref
void Build::traceUsingRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // The command may be saving its first handle to a reference, or it could be a duplicate of an
  // existing reference. Only emit the IR step for the first open.
  if (c->currentRun()->usingRef(ref)) {
    // This is an actual IR step, so count it
    stats::traced_steps++;

    // Create an IR step in the output trace
    _output.usingRef(c, ref);

    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::UsingRefPrinter{c, ref};
  }
}

// A command is finished using a Ref
void Build::traceDoneWithRef(const shared_ptr<Command>& c, Ref::ID ref_id) noexcept {
  // The command might be closing its last handle to the reference, or it could just be one of
  // several remaining handles. Use the returned refcount to catch the last close operation
  if (c->currentRun()->doneWithRef(ref_id)) {
    // This is an actual IR step, so count it
    stats::traced_steps++;

    // If this is the final use of the ref, inform the referenced artifact of the close
    auto ref = c->currentRun()->getRef(ref_id);
    if (ref->getUserCount() == 0) {
      auto a = ref->getArtifact();
      if (a) {
        LOG(exec) << c << " closing final ref to " << a << " with flags " << ref->getFlags();
        a->beforeClose(*this, c, ref_id);
      }
    }

    // Create an IR step in the output trace
    _output.doneWithRef(c, ref_id);

    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::DoneWithRefPrinter{c, ref_id};
  }
}

// Command c expects two references to compare with a specific result
void Build::traceCompareRefs(const shared_ptr<Command>& c,
                             Ref::ID ref1,
                             Ref::ID ref2,
                             RefComparison type) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // Create an IR step and add it to the output trace
  _output.compareRefs(c, ref1, ref2, type);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::CompareRefsPrinter{c, ref1, ref2, type};
}

// Command c expects a reference to resolve with a specific result as observed from the trace
void Build::traceExpectResult(const shared_ptr<Command>& c, Ref::ID ref_id, int expected) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto ref = c->currentRun()->getRef(ref_id);

  // If no expected result was provided, use the result from the reference itself
  if (expected == -1) expected = ref->getResultCode();

  // Create an IR step and add it to the output trace
  _output.expectResult(c, Scenario::Build, ref_id, expected);

  // Check the expected (i.e., observed) result against our filesystem model
  WARN_IF(ref->getResultCode() != expected)
      << "Reference resolved to " << getErrorName(ref->getResultCode())
      << ", which does not match syscall result " << getErrorName(expected);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::ExpectResultPrinter{c, Scenario::Build, ref_id, expected};
}

// Command c accesses an artifact's metadata
shared_ptr<MetadataVersion> Build::traceMatchMetadata(const shared_ptr<Command>& c,
                                                      Ref::ID ref_id) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto ref = c->currentRun()->getRef(ref_id);

  // Get the artifact whose metadata is being accessed
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to access metadata through unresolved reference " << ref;

  // Get the current metadata from the artifact
  auto expected = artifact->getMetadata(c, InputType::Accessed);
  ASSERT(expected) << "Unable to get metadata from " << artifact;

  // Create an IR step and add it to the output trace
  _output.matchMetadata(c, Scenario::Build, ref_id, expected);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::MatchMetadataPrinter{c, Scenario::Build, ref_id, expected};

  return expected;
}

// Command c accesses an artifact's content
void Build::traceMatchContent(const shared_ptr<Command>& c,
                              Ref::ID ref_id,
                              shared_ptr<ContentVersion> expected) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto ref = c->currentRun()->getRef(ref_id);

  // Get the artifact whose content is being accessed
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to access content through an unresolved reference " << ref;

  ASSERT(expected) << "Attempted to match contenet of " << artifact << " against a null version";

  // Create an IR step and add it to the output trace
  _output.matchContent(c, Scenario::Build, ref_id, expected);

  // Get a path to this artifact
  auto path = artifact->getPath(false);

  // If the artifact has a committed path, we may fingerprint or cache it
  if (path.has_value()) {
    auto fingerprint_type = policy::chooseFingerprintType(c, expected->getCreator(), path.value());
    expected->fingerprint(path.value(), fingerprint_type);

    // cache?
    if (policy::isCacheable(c, expected->getCreator(), path.value())) {
      expected->cache(path.value());
    }
  }

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::MatchContentPrinter{c, Scenario::Build, ref_id, expected};
}

// Command c modifies an artifact
void Build::traceUpdateMetadata(const shared_ptr<Command>& c,
                                Ref::ID ref_id,
                                shared_ptr<MetadataVersion> written) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto ref = c->currentRun()->getRef(ref_id);

  // Get the artifact whose metadata is being written
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to write metadata through an unresolved reference " << ref;

  // Record the update in the artifact
  ASSERT(written) << "Tried to write a null metadata version to " << artifact;
  artifact->updateMetadata(c, written);

  // Create an IR step and add it to the output trace
  _output.updateMetadata(c, ref_id, written);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::UpdateMetadataPrinter{c, ref_id, written};
}

// Command c modifies an artifact
void Build::traceUpdateContent(const shared_ptr<Command>& c,
                               Ref::ID ref_id,
                               shared_ptr<ContentVersion> written) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto ref = c->currentRun()->getRef(ref_id);

  // Get the artifact whose content is being written
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to write content through an unresolved reference " << ref;

  // Make sure we were given a version to write
  ASSERT(written) << "Attempted to write null version to " << artifact;

  // Create an IR step and add it to the output trace
  _output.updateContent(c, ref_id, written);

  // The calling command created this version
  written->createdBy(c);

  // Update the artifact's content
  artifact->updateContent(c, written);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::UpdateContentPrinter{c, ref_id, written};
}

// A traced command is adding an entry to a directory
void Build::traceAddEntry(const shared_ptr<Command>& c,
                          Ref::ID dir_id,
                          fs::path name,
                          Ref::ID target_id) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto dir = c->currentRun()->getRef(dir_id);
  auto target = c->currentRun()->getRef(target_id);

  // Get the directory artifact that is being added to
  auto dir_artifact = dir->getArtifact();
  ASSERT(dir_artifact) << "Tried to add an entry to an unresolved reference";

  // Make sure the reference to the artifact being linked is resolved
  ASSERT(target->isResolved()) << "Cannot add entry " << name << " to " << dir_artifact
                               << " using unresolved reference " << target;

  // Create an IR step and add it to the output trace
  _output.addEntry(c, dir_id, name, target_id);

  // Add the entry to the directory
  dir_artifact->addEntry(c, name, target->getArtifact());

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::AddEntryPrinter{c, dir_id, name, target_id};
}

// A traced command is removing an entry from a directory
void Build::traceRemoveEntry(const shared_ptr<Command>& c,
                             Ref::ID dir_id,
                             fs::path name,
                             Ref::ID target_id) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto dir = c->currentRun()->getRef(dir_id);
  auto target = c->currentRun()->getRef(target_id);

  // Get the directory artifact that is being removed from
  auto dir_artifact = dir->getArtifact();
  ASSERT(dir_artifact) << "Tried to add an entry to an unresolved reference";

  // Make sure the reference to the artifact being linked is resolved
  ASSERT(target->isResolved()) << "Cannot remove entry " << name << " from " << dir_artifact
                               << " using unresolved reference " << target;

  // Create an IR step and add it to the output trace
  _output.removeEntry(c, dir_id, name, target_id);

  // Remove the entry from the directory
  dir_artifact->removeEntry(c, name, target->getArtifact());

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::RemoveEntryPrinter{c, dir_id, name, target_id};
}

// This command launches a child command
shared_ptr<Command> Build::traceLaunch(const shared_ptr<Command>& parent,
                                       vector<string> args,
                                       Ref::ID exe_ref,
                                       Ref::ID cwd_ref,
                                       Ref::ID root_ref,
                                       map<int, Ref::ID> fds) noexcept {
  // Count a traced step and a traced command
  stats::traced_steps++;
  stats::traced_commands++;

  // Look to see if the current command has a matching child command
  auto child = parent->previousRun()->findChild(args, exe_ref, cwd_ref, root_ref, fds);

  // Did we find a matching command?
  if (child) {
    LOG(exec) << "Matched command " << child;
  } else {
    child = make_shared<Command>(args);
    LOG(exec) << "No match for command " << child;
  }

  // Remember that child command executed
  _commands.insert(child);

  // Mark the child command so we know it's currently running
  child->setMarking(RebuildMarking::MustRun);

  // Add the child to the parent's list of children
  parent->currentRun()->addChild(child);

  // Build a mapping from parent refs to child refs to emit to the IR layer
  list<tuple<Ref::ID, Ref::ID>> refs;

  // Add standard references to the child and record them in the refs list
  child->currentRun()->setRef(Ref::Root, parent->currentRun()->getRef(root_ref));
  refs.emplace_back(root_ref, Ref::Root);

  child->currentRun()->setRef(Ref::Cwd, parent->currentRun()->getRef(cwd_ref));
  refs.emplace_back(cwd_ref, Ref::Cwd);

  child->currentRun()->setRef(Ref::Exe, parent->currentRun()->getRef(exe_ref));
  refs.emplace_back(exe_ref, Ref::Exe);

  // Add references for initial file descriptors
  for (const auto& [fd, parent_ref] : fds) {
    auto child_ref = child->currentRun()->setRef(parent->currentRun()->getRef(parent_ref));
    child->addInitialFD(fd, child_ref);
    refs.emplace_back(parent_ref, child_ref);

    LOG(exec) << child << " inherits fd " << fd << " from parent ref " << parent_ref
              << ", now using child ref " << child_ref;
  }

  // Prepare the child command to execute by committing the necessary state from its references
  child->currentRun()->createLaunchDependencies(*this);

  // The child command will be executed by this build.
  child->setExecuted();

  // Create an IR step and add it to the output trace
  _output.launch(parent, child, refs);

  // Show the command if printing is on, or if this is a dry run
  if (options::print_on_run) {
    cout << child->getShortName(options::command_length) << endl;
  }

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::LaunchPrinter{parent, child, refs};

  // Now that the child has been launched, record that it is using all of its inherited refs
  for (const auto& [parent_ref_id, child_ref_id] : refs) {
    traceUsingRef(child, child_ref_id);
  }

  // Return the child command to the caller
  return child;
}

// This command joined with a child command
void Build::traceJoin(const shared_ptr<Command>& c,
                      const shared_ptr<Command>& child,
                      int exit_status) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // Create an IR step and add it to the output trace
  _output.join(c, child, exit_status);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::JoinPrinter{c, child, exit_status};
}

void Build::traceExit(const shared_ptr<Command>& c, int exit_status) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // Create an IR step and add it to the output trace
  _output.exit(c, exit_status);

  // Save the exit status for this command
  c->currentRun()->setExitStatus(exit_status);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::ExitPrinter{c, exit_status};
}
