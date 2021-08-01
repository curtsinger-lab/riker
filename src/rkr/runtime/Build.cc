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
#include "data/IRBuffer.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "runtime/env.hh"
#include "runtime/policy.hh"
#include "tracing/Tracer.hh"
#include "util/TracePrinter.hh"
#include "util/log.hh"
#include "util/options.hh"
#include "util/stats.hh"
#include "util/wrappers.hh"
#include "versions/ContentVersion.hh"
#include "versions/DirVersion.hh"
#include "versions/MetadataVersion.hh"

using std::cout;
using std::endl;
using std::list;
using std::make_shared;
using std::make_unique;
using std::map;
using std::set;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

namespace fs = std::filesystem;

// Create a build runner
Build::Build(IRSink& output, std::shared_ptr<std::ostream> print_to) noexcept :
    _output(output), _tracer(*this), _print_to(print_to) {
  _deferred_steps = make_unique<IRBuffer>();
}

void Build::runDeferredSteps() noexcept {
  // Create a new deferral buffer and swap it with the existing deferred steps
  auto to_run = make_unique<IRBuffer>();
  std::swap(to_run, _deferred_steps);

  // Feed all deferred IR steps back through for emulation
  to_run->sendTo(*this);
}

/************************ Handle IR steps from a loaded trace ************************/

/// Start a build with the given root command
void Build::start(const shared_ptr<Command>& c) noexcept {
  _root_command = c;

  // The root command is launched
  _root_command->setLaunched();

  // Pass the root command on to the output
  _output.start(_root_command);
}

void Build::finish() noexcept {
  // Wait for all remaining processes to exit
  _tracer.wait();

  // Compare the final state of all artifacts to the actual filesystem
  env::getRootDir()->checkFinalState("/");

  // Finish the run of the root command and all descendants (recursively)
  _root_command->finishRun();

  // Inform the output trace that it is finished
  _output.finish();

  // This build no longer has a root command
  _root_command.reset();
}

void Build::specialRef(const shared_ptr<Command>& c, SpecialRef entity, Ref::ID output) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->specialRef(c, entity, output);
    return;
  }

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
    c->setRef(output, stdin_ref);

  } else if (entity == SpecialRef::stdout) {
    // Create the stdout ref and add one user (the build tool)
    auto stdout_ref = make_shared<Ref>(WriteAccess, env::getStdout(c));
    stdout_ref->addUser();
    c->setRef(output, stdout_ref);

  } else if (entity == SpecialRef::stderr) {
    // Create the stderr ref and add one user (the build tool)
    auto stderr_ref = make_shared<Ref>(WriteAccess, env::getStderr(c));
    stderr_ref->addUser();
    c->setRef(output, stderr_ref);

  } else if (entity == SpecialRef::root) {
    c->setRef(output, make_shared<Ref>(ReadAccess + ExecAccess, env::getRootDir()));

  } else if (entity == SpecialRef::cwd) {
    auto cwd_path = fs::current_path().relative_path();
    auto ref = make_shared<Ref>(env::getRootDir()->resolve(c, cwd_path, ReadAccess + ExecAccess));
    c->setRef(output, ref);

    ASSERT(ref->isSuccess()) << "Failed to resolve current working directory";
    ref->getArtifact()->setName(".");

  } else if (entity == SpecialRef::launch_exe) {
    auto rkr = readlink("/proc/self/exe");
    auto rkr_launch = (rkr.parent_path() / "rkr-launch").relative_path();

    auto ref = make_shared<Ref>(env::getRootDir()->resolve(c, rkr_launch, ReadAccess + ExecAccess));
    c->setRef(output, ref);

  } else {
    FAIL << "Unknown special reference";
  }
}

// A command references a new anonymous pipe
void Build::pipeRef(const shared_ptr<Command>& c, Ref::ID read_end, Ref::ID write_end) noexcept {
  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::PipeRefPrinter{c, read_end, write_end};

  } else {
    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::PipeRefPrinter{c, read_end, write_end};

    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps->pipeRef(c, read_end, write_end);
      return;
    }
  }

  // Create an IR step and add it to the output trace
  _output.pipeRef(c, read_end, write_end);

  // Resolve the reference and save the result in output
  auto pipe = env::getPipe(c);
  c->setRef(read_end, make_shared<Ref>(ReadAccess, pipe));
  c->setRef(write_end, make_shared<Ref>(WriteAccess, pipe));
}

// A command references a new anonymous file
void Build::fileRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept {
  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::FileRefPrinter{c, mode, output};

  } else {
    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::FileRefPrinter{c, mode, output};

    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps->fileRef(c, mode, output);
      return;
    }
  }

  // Create an IR step and add it to the output trace
  _output.fileRef(c, mode, output);

  // Resolve the reference and save the result in output
  c->setRef(output, make_shared<Ref>(ReadAccess + WriteAccess, env::createFile(c, mode)));
}

// A command references a new anonymous symlink
void Build::symlinkRef(const shared_ptr<Command>& c, fs::path target, Ref::ID output) noexcept {
  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::SymlinkRefPrinter{c, target, output};

  } else {
    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::SymlinkRefPrinter{c, target, output};

    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps->symlinkRef(c, target, output);
      return;
    }
  }

  // Create an IR step and add it to the output trace
  _output.symlinkRef(c, target, output);

  // Resolve the reference and save the result in output
  c->setRef(output,
            make_shared<Ref>(ReadAccess + WriteAccess + ExecAccess, env::getSymlink(c, target)));
}

// A command references a new anonymous directory
void Build::dirRef(const shared_ptr<Command>& c, mode_t mode, Ref::ID output) noexcept {
  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::DirRefPrinter{c, mode, output};

  } else {
    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::DirRefPrinter{c, mode, output};

    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps->dirRef(c, mode, output);
      return;
    }
  }

  // Create an IR step and add it to the output trace
  _output.dirRef(c, mode, output);

  // Resolve the reference and save the result in output
  c->setRef(output, make_shared<Ref>(ReadAccess + WriteAccess + ExecAccess, env::getDir(c, mode)));
}

// A command makes a reference with a path
void Build::pathRef(const shared_ptr<Command>& c,
                    Ref::ID base,
                    fs::path path,
                    AccessFlags flags,
                    Ref::ID output) noexcept {
  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::PathRefPrinter{c, base, path, flags, output};

  } else {
    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::PathRefPrinter{c, base, path, flags, output};

    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps->pathRef(c, base, path, flags, output);
      return;
    }
  }

  // Get the directory where resolution should begin
  auto base_dir = c->getRef(base)->getArtifact();

  // Is this a path to a temporary file?
  bool is_tempfile = false;
  if (base_dir == env::getRootDir() && path.string().substr(0, 4) == "tmp/") {
    is_tempfile = true;

    // The command may be running different temporary file paths. Substitute the path now
    string newpath = c->substitutePath("/" + path.string());
    path = fs::path(newpath.substr(1));
  }

  // Create an IR step and add it to the output trace
  _output.pathRef(c, base, path, flags, output);

  // Is the base directory available?
  if (!base_dir) {
    c->observeChange(Scenario::Both);
    return;
  }

  // Resolve the reference
  shared_ptr<Ref> result = make_shared<Ref>(base_dir->resolve(c, path, flags));

  // If this reference was to a temporary file, inform the command
  if (result->isSuccess() && is_tempfile) c->addTempfile(result->getArtifact());

  // Assign to the command's reference
  c->setRef(output, result);
}

// A command retains a handle to a given Ref
void Build::usingRef(const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::UsingRefPrinter{c, ref};

  } else {
    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::UsingRefPrinter{c, ref};

    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps->usingRef(c, ref);
      return;
    }
  }

  // Command c is now using ref
  c->usingRef(ref);

  // Create an IR step and add it to the output trace
  _output.usingRef(c, ref);
}

// A command closes a handle to a given Ref
void Build::doneWithRef(const shared_ptr<Command>& c, Ref::ID ref_id) noexcept {
  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::DoneWithRefPrinter{c, ref_id};

  } else {
    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::DoneWithRefPrinter{c, ref_id};

    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps->doneWithRef(c, ref_id);
      return;
    }
  }

  // Command c is no longer using ref
  c->doneWithRef(ref_id);

  // If this is the final use of the ref, inform the referenced artifact of the close
  auto ref = c->getRef(ref_id);
  if (ref->getUserCount() == 0) {
    auto a = ref->getArtifact();
    if (a) {
      a->beforeClose(*this, c, ref_id);
    }
  }

  // Create an IR step and add it to the output trace
  _output.doneWithRef(c, ref_id);
}

// Command c depends on the outcome of comparing two different references
void Build::compareRefs(const shared_ptr<Command>& c,
                        Ref::ID ref1_id,
                        Ref::ID ref2_id,
                        RefComparison type) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->compareRefs(c, ref1_id, ref2_id, type);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::CompareRefsPrinter{c, ref1_id, ref2_id, type};

  // Create an IR step and add it to the output trace
  _output.compareRefs(c, ref1_id, ref2_id, type);

  auto ref1 = c->getRef(ref1_id);
  auto ref2 = c->getRef(ref2_id);

  // Does the comparison resolve as expected?
  if (type == RefComparison::SameInstance) {
    if (ref1->getArtifact() != ref2->getArtifact()) {
      LOGF(rebuild, "{} changed: expected {} and {} to refer to the same artifact", c, ref1, ref2);

      c->observeChange(Scenario::Both);
    }
  } else if (type == RefComparison::DifferentInstances) {
    if (ref1->getArtifact() == ref2->getArtifact()) {
      LOGF(rebuild, "{} changed: expected {} and {} to refer to different artifacts", c, ref1,
           ref2);

      c->observeChange(Scenario::Both);
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
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->expectResult(c, scenario, ref_id, expected);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::ExpectResultPrinter{c, scenario, ref_id, expected};

  // Create an IR step and add it to the output trace
  _output.expectResult(c, scenario, ref_id, expected);

  // Does the resolved reference match the expected result?
  auto ref = c->getRef(ref_id);
  if (ref->getResultCode() != expected) {
    LOGF(rebuild,
         "{} changed in scenario {}: r{}={} did not resolve as expected (expected {}, observed "
         "{})",
         c, scenario, ref_id, ref, expected, ref->getResultCode());
    c->observeChange(scenario);
  }
}

// Command c accesses an artifact's metadata
void Build::matchMetadata(const shared_ptr<Command>& c,
                          Scenario scenario,
                          Ref::ID ref_id,
                          MetadataVersion expected) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->matchMetadata(c, scenario, ref_id, expected);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::MatchMetadataPrinter{c, scenario, ref_id, expected};

  // Create an IR step and add it to the output trace
  _output.matchMetadata(c, scenario, ref_id, expected);

  auto ref = c->getRef(ref_id);

  // We can't do anything with an unresolved reference. A change should already have been
  // reported.
  if (!ref->isResolved()) return;

  // Perform the comparison
  ref->getArtifact()->matchMetadata(c, scenario, expected);
}

// Command c accesses an artifact's content
void Build::matchContent(const shared_ptr<Command>& c,
                         Scenario scenario,
                         Ref::ID ref_id,
                         shared_ptr<ContentVersion> expected) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->matchContent(c, scenario, ref_id, expected);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::MatchContentPrinter{c, scenario, ref_id, expected};

  // Create an IR step and add it to the output trace
  _output.matchContent(c, scenario, ref_id, expected);

  auto ref = c->getRef(ref_id);

  // We can't do anything with an unresolved reference. A change should already have been
  // reported.
  if (!ref->isResolved()) return;

  // Perform the comparison
  ref->getArtifact()->matchContent(c, scenario, expected);
}

// Command c modifies an artifact
void Build::updateMetadata(const shared_ptr<Command>& c,
                           Ref::ID ref_id,
                           MetadataVersion written) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->updateMetadata(c, ref_id, written);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::UpdateMetadataPrinter{c, ref_id, written};

  // Create an IR step and add it to the output trace
  _output.updateMetadata(c, ref_id, written);

  auto ref = c->getRef(ref_id);

  // We can't do anything with an unresolved reference. A change should already have been
  // reported.
  if (!ref->isResolved()) return;

  // Apply the write
  ref->getArtifact()->updateMetadata(c, written);
}

// Command c modifies an artifact
void Build::updateContent(const shared_ptr<Command>& c,
                          Ref::ID ref_id,
                          shared_ptr<ContentVersion> written) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->updateContent(c, ref_id, written);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::UpdateContentPrinter{c, ref_id, written};

  // Create an IR step and add it to the output trace
  _output.updateContent(c, ref_id, written);

  auto ref = c->getRef(ref_id);

  // We can't do anything with an unresolved reference. A change should already have been
  // reported.
  if (!ref->isResolved()) return;

  // Apply the write
  ref->getArtifact()->updateContent(c, written);
}

/// Handle an AddEntry IR step
void Build::addEntry(const shared_ptr<Command>& c,
                     Ref::ID dir_id,
                     string name,
                     Ref::ID target_id) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->addEntry(c, dir_id, name, target_id);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::AddEntryPrinter{c, dir_id, name, target_id};

  // Create an IR step and add it to the output trace
  _output.addEntry(c, dir_id, name, target_id);

  auto dir = c->getRef(dir_id);
  auto target = c->getRef(target_id);

  // We can't do anything with unresolved references. A change should already have been reported.
  if (!dir->isResolved() || !target->isResolved()) return;

  // Add the entry to the directory
  dir->getArtifact()->addEntry(c, name, target->getArtifact());
}

/// Handle a RemoveEntry IR step
void Build::removeEntry(const shared_ptr<Command>& c,
                        Ref::ID dir_id,
                        string name,
                        Ref::ID target_id) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->removeEntry(c, dir_id, name, target_id);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::RemoveEntryPrinter{c, dir_id, name, target_id};

  // Create an IR step and add it to the output trace
  _output.removeEntry(c, dir_id, name, target_id);

  auto dir = c->getRef(dir_id);
  auto target = c->getRef(target_id);

  // We can't do anything with unresolved references. A change should already have been reported.
  if (!dir->isResolved() || !target->isResolved()) return;

  // Remove the entry from the directory
  dir->getArtifact()->removeEntry(c, name, target->getArtifact());
}

// This command launches a child command
void Build::launch(const shared_ptr<Command>& c,
                   const shared_ptr<Command>& child,
                   list<tuple<Ref::ID, Ref::ID>> refs) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) {
    // If the child command doesn't have to run, add it to the deferred command set
    if (!child->mustRun()) _deferred_commands.insert(child);

    // We're not going to emulate this command because it has to run
    return;
  }

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);

    // Record the launch step in the trace of deferred steps
    _deferred_steps->launch(c, child, refs);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::LaunchPrinter{c, child, refs};

  // Assign references in the child command
  for (const auto& [parent_ref_id, child_ref_id] : refs) {
    child->setRef(child_ref_id, c->getRef(parent_ref_id));

    // The child will have access to the artifact's content, so create a dependency if needed
    auto a = c->getRef(parent_ref_id)->getArtifact();
    if (a) a->getContent(child);
  }

  // Add the child to the parent command's set of children
  c->addChild(child);

  // Are we going to re-execute the child?
  bool run_command = false;

  // Should we print the child command?
  bool print_command = false;

  // Is this command marked for rerun?
  if (child->mustRun()) {
    // Print the command if requested, or if this is a dry run
    if (options::print_on_run || options::dry_run) print_command = true;

    // Launch the command if this is not a dry run
    if (!options::dry_run) run_command = true;

  } else if (!child->hasExecuted()) {
    // The command is not running now, and has never run before. Ensure it is marked.
    LOGF(rebuild, "{} changed: never run", child);
    child->observeChange(Scenario::Both);
  }

  // Print the command if requested
  if (print_command) {
    if (options::print_full) {
      if (_print_to) {
        (*_print_to) << child->getFullName() << endl;
      } else {
        cout << child->getFullName() << endl;
      }
    } else {
      if (_print_to) {
        (*_print_to) << child->getShortName(options::command_length) << endl;
      } else {
        cout << child->getShortName(options::command_length) << endl;
      }
    }
  }

  // If we're going to actually run the command, mark it as executed now
  if (run_command) child->setExecuted();

  // Now emit the launch IR step. This has to happen after updating the executed state of the
  // command (above) and before actually launching the command.
  _output.launch(c, child, refs);

  // Launch the command if requested
  if (run_command) {
    // Count the traced command
    stats::traced_commands++;

    // Prepare the child command to execute by committing the necessary state from its references
    child->createLaunchDependencies();

    LOG(exec) << c << " is launching " << child << " as a traced command";

    // Start the child command in the tracer and record it as launched
    const auto& process = _tracer.start(child);
    child->setLaunched(process);

  } else {
    // Count the emulated command
    stats::emulated_commands++;

    LOG(exec) << c << " is launching " << child << " as an emulated command";

    // The child command has launched with no containing process
    child->setLaunched();
  }
}

// This command joined with a child command
void Build::join(const shared_ptr<Command>& c,
                 const shared_ptr<Command>& child,
                 int exit_status) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->join(c, child, exit_status);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // If the child command is running in the tracer, wait for it
  const auto& process = child->getProcess();
  if (process) _tracer.wait(process);

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::JoinPrinter{c, child, exit_status};

  // Create an IR step and add it to the output trace
  _output.join(c, child, exit_status);

  // Did the child command's exit status match the expected result?
  if (child->getExitStatus() != exit_status) {
    LOGF(rebuild, "{} changed: child {} exited with different status (expected {}, observed {})", c,
         child, exit_status, child->getExitStatus());

    // The command detects a changed exit status from its child, so it must rerun
    c->observeChange(Scenario::Both);
  }
}

void Build::exit(const shared_ptr<Command>& c, int exit_status) noexcept {
  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;

  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps->exit(c, exit_status);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::ExitPrinter{c, exit_status};

  // Create an IR step and add it to the output trace
  _output.exit(c, exit_status);

  // Save the exit status for this command
  c->setExitStatus(exit_status);

  // Is this emulated command running in a process? If so, we need to let it exit
  const auto& process = c->getProcess();
  if (process) {
    process->forceExit(exit_status);
  }
}

/************************ Trace IR Steps ************************/

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

  auto ref = c->getRef(ref_id);

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
MetadataVersion Build::traceMatchMetadata(const shared_ptr<Command>& c, Ref::ID ref_id) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto ref = c->getRef(ref_id);

  // Get the artifact whose metadata is being accessed
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to access metadata through unresolved reference " << ref;

  // Get the current metadata from the artifact
  auto expected = artifact->getMetadata(c);

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

  auto ref = c->getRef(ref_id);

  // Get the artifact whose content is being accessed
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to access content through an unresolved reference " << ref;

  ASSERT(expected) << "Attempted to match content of " << artifact << " against a null version";

  // Create an IR step and add it to the output trace
  _output.matchContent(c, Scenario::Build, ref_id, expected);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::MatchContentPrinter{c, Scenario::Build, ref_id, expected};
}

// Command c modifies an artifact
void Build::traceUpdateMetadata(const shared_ptr<Command>& c,
                                Ref::ID ref_id,
                                MetadataVersion written) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto ref = c->getRef(ref_id);

  // Get the artifact whose metadata is being written
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to write metadata through an unresolved reference " << ref;

  // Record the update in the artifact
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

  auto ref = c->getRef(ref_id);

  // Get the artifact whose content is being written
  auto artifact = ref->getArtifact();
  ASSERT(artifact) << "Tried to write content through an unresolved reference " << ref;

  // Make sure we were given a version to write
  ASSERT(written) << "Attempted to write null version to " << artifact;

  // Create an IR step and add it to the output trace
  _output.updateContent(c, ref_id, written);

  // Update the artifact's content
  artifact->updateContent(c, written);

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::UpdateContentPrinter{c, ref_id, written};
}

// A traced command is adding an entry to a directory
void Build::traceAddEntry(const shared_ptr<Command>& c,
                          Ref::ID dir_id,
                          string name,
                          Ref::ID target_id) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto dir = c->getRef(dir_id);
  auto target = c->getRef(target_id);

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
                             string name,
                             Ref::ID target_id) noexcept {
  // Count a traced step
  stats::traced_steps++;

  auto dir = c->getRef(dir_id);
  auto target = c->getRef(target_id);

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
                                       const map<int, Ref::ID>& fds,
                                       shared_ptr<Process> process) noexcept {
  // Count a traced step
  stats::traced_steps++;

  // We're going to hunt for a command that matches this launch. Keep track of the best match.
  shared_ptr<Command> child = nullptr;

  // Matches may require path substitutions for temporary files. Remember those.
  map<string, string> child_substitutions;

  // TODO: Should tempfile substitutions be global? Probably. For now they are unique to each
  // command, which could cause problems in strange cases.

  // Loop over the set of deferred commands
  for (const auto& candidate : _deferred_commands) {
    // Has the candidate been launched already? If so we cannot match it
    if (candidate->isLaunched()) continue;

    // Prefer matches marked Emulate over MayRun or MustRun
    if (child && child->getMarking() <= candidate->getMarking()) continue;

    // Try to match the candidate to the given arguments
    auto substitutions = candidate->tryToMatch(args, fds);

    // If there was no match, continue
    if (!substitutions.has_value()) continue;

    // We have a match. If we made it this far it must be better than the previous match
    child = candidate;
    child_substitutions = std::move(substitutions.value());
  }

  // Build a mapping from parent refs to child refs to emit to the IR layer
  list<tuple<Ref::ID, Ref::ID>> refs;

  // Did we find a matching command?
  if (child) {
    // Remove the child from the deferred command set
    _deferred_commands.erase(child);

    // Update the initial FDs for the child, which might point to different references now
    // child->setInitialFDs(fds);

    // We found a matching child command. Apply the required substitutions
    child->applySubstitutions(child_substitutions);

    LOG(exec) << "Matched launch of " << child << " by " << parent;

    // Add standard references to the child and record them in the refs list
    child->setRef(Ref::Root, parent->getRef(root_ref));
    refs.emplace_back(root_ref, Ref::Root);

    child->setRef(Ref::Cwd, parent->getRef(cwd_ref));
    refs.emplace_back(cwd_ref, Ref::Cwd);

    child->setRef(Ref::Exe, parent->getRef(exe_ref));
    refs.emplace_back(exe_ref, Ref::Exe);

    // Set references for initial file descriptors
    for (const auto& [fd, child_ref] : child->getInitialFDs()) {
      auto parent_ref = fds.at(fd);
      child->setRef(child_ref, parent->getRef(parent_ref));
      refs.emplace_back(parent_ref, child_ref);
    }

  } else {
    // Create a child and mark it as running
    child = make_shared<Command>(args);
    child->setMarking(RebuildMarking::MustRun);

    LOG(exec) << "New command " << child << " did not match any previous command.";

    // Add standard references to the child and record them in the refs list
    child->setRef(Ref::Root, parent->getRef(root_ref));
    refs.emplace_back(root_ref, Ref::Root);

    child->setRef(Ref::Cwd, parent->getRef(cwd_ref));
    refs.emplace_back(cwd_ref, Ref::Cwd);

    child->setRef(Ref::Exe, parent->getRef(exe_ref));
    refs.emplace_back(exe_ref, Ref::Exe);

    // Add references for initial file descriptors
    for (const auto& [fd, parent_ref] : fds) {
      auto child_ref = child->setRef(parent->getRef(parent_ref));
      child->addInitialFD(fd, child_ref);
      refs.emplace_back(parent_ref, child_ref);
    }
  }

  // Add the child to the parent's list of children
  parent->addChild(child);

  // Prepare the child command to execute by committing the necessary state from its references
  child->createLaunchDependencies();

  // The command will be executed
  if (child->mustRun()) child->setExecuted();

  // The child command is now launched in the provided process
  child->setLaunched(process);

  // Create an IR step and add it to the output trace
  _output.launch(parent, child, refs);

  // Does the command have to run? Update the appropriate stats counter
  if (child->mustRun()) {
    stats::traced_commands++;
  } else {
    stats::emulated_commands++;
  }

  // Print the command if required
  if (child->mustRun() && options::print_on_run) {
    if (options::print_full) {
      if (_print_to) {
        (*_print_to) << child->getFullName() << endl;
      } else {
        cout << child->getFullName() << endl;
      }
    } else {
      if (_print_to) {
        (*_print_to) << child->getShortName(options::command_length) << endl;
      } else {
        cout << child->getShortName(options::command_length) << endl;
      }
    }
  }

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::LaunchPrinter{parent, child, refs};

  // Now that the child has been launched, record that it is using all of its inherited refs
  for (const auto& [parent_ref_id, child_ref_id] : refs) {
    usingRef(child, child_ref_id);
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
  c->setExitStatus(exit_status);

  // Cache and fingerprint everything in the environment at the end of this command's run
  env::cacheAll();

  // Log the traced step
  LOG(ir) << "traced " << TracePrinter::ExitPrinter{c, exit_status};
}
