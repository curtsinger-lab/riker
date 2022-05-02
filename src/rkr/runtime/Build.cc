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
#include "data/IRSource.hh"
#include "data/Trace.hh"
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
Build::Build(IRSink& output, std::ostream& print_to) noexcept :
    _output(output), _print_to(print_to) {}

void Build::runDeferredSteps() noexcept {
  // Create a TraceReader to emit deferred steps
  auto input = _deferred_steps.getReader();

  // Create a new buffer to hold new deferred steps
  _deferred_steps = TraceWriter();

  LOG(exec) << "runDeferredSteps";
  // Feed all deferred IR steps back through for emulation
  input.sendTo(*this);

}

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
  _tracer.wait(*this);
  
  // Compare the final state of all artifacts to the actual filesystem
  env::getRootDir()->checkFinalState("/");
  
  // Finish the run of the root command and all descendants (recursively)
  _root_command->finishRun();
  
  // Inform the output trace that it is finished
  _output.finish();
  
  // This build no longer has a root command
  _root_command.reset();
  
}

void Build::specialRef(const IRSource& source,
                       const shared_ptr<Command>& c,
                       SpecialRef entity,
                       Ref::ID output) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // If this step comes from a command we need to run, return immediately
  if (c->mustRun()) return;
  
  if (c->isOrphaned()) {
    _output.specialRef(c, entity, output);
    return;
  }
  
  // If this step comes from a command that hasn't been launched, we need to defer this step
  if (!c->isLaunched()) {
    _deferred_commands.emplace(c);
    _deferred_steps.specialRef(source, c, entity, output);
    return;
  }

  // Count an emulated step
  stats::emulated_steps++;

  // Log the emulated step
  LOG(ir) << "emulated " << TracePrinter::SpecialRefPrinter{c, entity, output};

  // Create an IR step and add it to the output trace
  _output.specialRef(source, c, entity, output);

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
void Build::pipeRef(const IRSource& source,
                    const shared_ptr<Command>& c,
                    Ref::ID read_end,
                    Ref::ID write_end) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::PipeRefPrinter{c, read_end, write_end};

  } else {
    if (c->isOrphaned()) {
    _output.pipeRef(c, read_end, write_end);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.pipeRef(source, c, read_end, write_end);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::PipeRefPrinter{c, read_end, write_end};
  }

  // Create an IR step and add it to the output trace
  _output.pipeRef(source, c, read_end, write_end);

  // Resolve the reference and save the result in output
  auto pipe = env::getPipe(c);
  c->setRef(read_end, make_shared<Ref>(ReadAccess, pipe));
  c->setRef(write_end, make_shared<Ref>(WriteAccess, pipe));
}

// A command references a new anonymous file
void Build::fileRef(const IRSource& source,
                    const shared_ptr<Command>& c,
                    mode_t mode,
                    Ref::ID output) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::FileRefPrinter{c, mode, output};

  } else {
    if (c->isOrphaned()) {
    _output.fileRef(c, mode, output);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.fileRef(source, c, mode, output);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::FileRefPrinter{c, mode, output};
  }

  // Create an IR step and add it to the output trace
  _output.fileRef(source, c, mode, output);

  // Resolve the reference and save the result in output
  c->setRef(output, make_shared<Ref>(ReadAccess + WriteAccess, env::createFile(c, mode)));
}

// A command references a new anonymous symlink
void Build::symlinkRef(const IRSource& source,
                       const shared_ptr<Command>& c,
                       fs::path target,
                       Ref::ID output) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::SymlinkRefPrinter{c, target, output};

  } else {
    if (c->isOrphaned()) {
    _output.symlinkRef(c, target, output);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.symlinkRef(source, c, target, output);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::SymlinkRefPrinter{c, target, output};
  }

  // Create an IR step and add it to the output trace
  _output.symlinkRef(source, c, target, output);

  // Resolve the reference and save the result in output
  c->setRef(output,
            make_shared<Ref>(ReadAccess + WriteAccess + ExecAccess, env::getSymlink(c, target)));
}

// A command references a new anonymous directory
void Build::dirRef(const IRSource& source,
                   const shared_ptr<Command>& c,
                   mode_t mode,
                   Ref::ID output) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::DirRefPrinter{c, mode, output};

  } else {
    if (c->isOrphaned()) {
    _output.dirRef(c, mode, output);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.dirRef(source, c, mode, output);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::DirRefPrinter{c, mode, output};
  }

  // Create an IR step and add it to the output trace
  _output.dirRef(source, c, mode, output);

  // Resolve the reference and save the result in output
  c->setRef(output, make_shared<Ref>(ReadAccess + WriteAccess + ExecAccess, env::getDir(c, mode)));
}

// A command makes a reference with a path
void Build::pathRef(const IRSource& source,
                    const shared_ptr<Command>& c,
                    Ref::ID base,
                    fs::path path,
                    AccessFlags flags,
                    Ref::ID output) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::PathRefPrinter{c, base, path, flags, output};

  } else {
    if (c->isOrphaned()) {
    _output.pathRef(c, base, path, flags, output);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.pathRef(source, c, base, path, flags, output);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::PathRefPrinter{c, base, path, flags, output};
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
  _output.pathRef(source, c, base, path, flags, output);

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
void Build::usingRef(const IRSource& source, const shared_ptr<Command>& c, Ref::ID ref) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::UsingRefPrinter{c, ref};

  } else {
    if (c->isOrphaned()) {
    _output.usingRef(c, ref);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.usingRef(source, c, ref);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::UsingRefPrinter{c, ref};
  }

  // Command c is now using ref
  c->usingRef(ref);

  // Create an IR step and add it to the output trace
  _output.usingRef(source, c, ref);
}

// A command closes a handle to a given Ref
void Build::doneWithRef(const IRSource& source,
                        const shared_ptr<Command>& c,
                        Ref::ID ref_id) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::DoneWithRefPrinter{c, ref_id};

  } else {
    if (c->isOrphaned()) {
    _output.doneWithRef(c, ref_id);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.doneWithRef(source, c, ref_id);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::DoneWithRefPrinter{c, ref_id};
  }

  // Command c is no longer using ref
  c->doneWithRef(ref_id);

  // If this is the final use of the ref, inform the referenced artifact of the close
  auto ref = c->getRef(ref_id);
  if (ref->getUserCount() == 0) {
    auto a = ref->getArtifact();
    if (a) {
      a->beforeClose(*this, source, c, ref_id);
    }
  }

  // Create an IR step and add it to the output trace
  _output.doneWithRef(source, c, ref_id);
}

// Command c depends on the outcome of comparing two different references
void Build::compareRefs(const IRSource& source,
                        const shared_ptr<Command>& c,
                        Ref::ID ref1_id,
                        Ref::ID ref2_id,
                        RefComparison type) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::CompareRefsPrinter{c, ref1_id, ref2_id, type};

  } else {
    if (c->isOrphaned()) {
    _output.compareRefs(c, ref1_id, ref2_id, type);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.compareRefs(source, c, ref1_id, ref2_id, type);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::CompareRefsPrinter{c, ref1_id, ref2_id, type}; 
  }

  // Create an IR step and add it to the output trace
  _output.compareRefs(source, c, ref1_id, ref2_id, type);

  // TODO: No need to perform the comparison during tracing

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
void Build::expectResult(const IRSource& source,
                         const shared_ptr<Command>& c,
                         Scenario scenario,
                         Ref::ID ref_id,
                         int8_t expected) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::ExpectResultPrinter{c, scenario, ref_id, expected};

  } else {
    if (c->isOrphaned()) {
    _output.expectResult(c, scenario, ref_id, expected);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.expectResult(source, c, scenario, ref_id, expected);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::ExpectResultPrinter{c, scenario, ref_id, expected};
  }

  // Create an IR step and add it to the output trace
  _output.expectResult(source, c, scenario, ref_id, expected);

  // Get the reference outcome. Does it match the expected result?
  auto ref = c->getRef(ref_id);
  if (ref->getResultCode() != expected) {
    // Mismatch. If the command is being emulated we report a change. Otherwise just warn that
    // something is wrong with tracing (the model doesn't match actual syscall behavior)
    if (c->mustRun()) {
      WARN << "Reference resolved to " << getErrorName(ref->getResultCode())
           << ", which does not match syscall result " << getErrorName(expected);
    } else {
      LOGF(rebuild,
           "{} changed in scenario {}: r{}={} did not resolve as expected (expected {}, observed "
           "{})",
           c, scenario, ref_id, ref, expected, ref->getResultCode());
      c->observeChange(scenario);
    }
  }
}

// Command c accesses an artifact's metadata
void Build::matchMetadata(const IRSource& source,
                          const shared_ptr<Command>& c,
                          Scenario scenario,
                          Ref::ID ref_id,
                          MetadataVersion expected) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::MatchMetadataPrinter{c, scenario, ref_id, expected};

  } else {
    if (c->isOrphaned()) {
    _output.matchMetadata(c, scenario, ref_id, expected);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.matchMetadata(source, c, scenario, ref_id, expected);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::MatchMetadataPrinter{c, scenario, ref_id, expected};
  }

  // Create an IR step and add it to the output trace
  _output.matchMetadata(source, c, scenario, ref_id, expected);

  // Get the reference we're matching against
  auto ref = c->getRef(ref_id);

  // Are we tracing this command?
  if (c->mustRun()) {
    // Yes. Just make sure the reference is valid
    ASSERT(ref->isResolved()) << "Tried to access metadata through unresolved reference " << ref;

  } else {
    // No. Make sure the reference is resolved, and if it is, check the predicate
    if (ref->isResolved()) {
      ref->getArtifact()->matchMetadata(c, scenario, expected);
    } else {
      c->observeChange(scenario);
    }
  }
}

// Command c accesses an artifact's content
void Build::matchContent(const IRSource& source,
                         const shared_ptr<Command>& c,
                         Scenario scenario,
                         Ref::ID ref_id,
                         shared_ptr<ContentVersion> expected) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::MatchContentPrinter{c, scenario, ref_id, expected};

  } else {
    if (c->isOrphaned()) {
   _output.matchContent(c, scenario, ref_id, expected);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.matchContent(source, c, scenario, ref_id, expected);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::MatchContentPrinter{c, scenario, ref_id, expected};
  }

  // Create an IR step and add it to the output trace
  _output.matchContent(source, c, scenario, ref_id, expected);

  // If this command is being emulated, check the predicate
  if (c->canEmulate()) {
    auto ref = c->getRef(ref_id);
    if (ref->isResolved()) {
      // Perform the comparison
      ref->getArtifact()->matchContent(c, scenario, expected);
    } else {
      c->observeChange(scenario);
    }
  }
}

// Command c modifies an artifact
void Build::updateMetadata(const IRSource& source,
                           const shared_ptr<Command>& c,
                           Ref::ID ref_id,
                           MetadataVersion written) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::UpdateMetadataPrinter{c, ref_id, written};

  } else {
    if (c->isOrphaned()) {
    _output.updateMetadata(c, ref_id, written);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.updateMetadata(source, c, ref_id, written);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::UpdateMetadataPrinter{c, ref_id, written};
  }

  // Create an IR step and add it to the output trace
  _output.updateMetadata(source, c, ref_id, written);

  // Get the reference
  auto ref = c->getRef(ref_id);

  // Is the reference resolved?
  if (ref->isResolved()) {
    // Yes. Apply the write
    ref->getArtifact()->updateMetadata(c, written);

  } else {
    // No. Are we tracing or emulating?
    if (c->mustRun()) {
      // Tracing. Something bad has happened
      WARN << "Tried to write metadata through an unresolved reference " << ref;

    } else {
      // Emulating. Detect a change
      c->observeChange(Scenario::Build);
    }
  }
}

// Command c modifies an artifact
void Build::updateContent(const IRSource& source,
                          const shared_ptr<Command>& c,
                          Ref::ID ref_id,
                          shared_ptr<ContentVersion> written) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::UpdateContentPrinter{c, ref_id, written};

  } else {
    if (c->isOrphaned()) {
    _output.updateContent(c, ref_id, written);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.updateContent(source, c, ref_id, written);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::UpdateContentPrinter{c, ref_id, written};
  }

  // Create an IR step and add it to the output trace
  _output.updateContent(source, c, ref_id, written);

  // Get the reference being written through
  auto ref = c->getRef(ref_id);

  // Is the reference resolved?
  if (ref->isResolved()) {
    // Yes. Apply the write
    ref->getArtifact()->updateContent(c, written);

  } else {
    // No. Are we tracing or emulating?
    if (c->mustRun()) {
      // Tracing. Something bad has happened
      WARN << "Tried to write content through an unresolved reference " << ref;

    } else {
      // Emulating. Detect a change
      c->observeChange(Scenario::Build);
    }
  }
}

// Command c adds an entry to a directory
void Build::addEntry(const IRSource& source,
                     const shared_ptr<Command>& c,
                     Ref::ID dir_id,
                     string name,
                     Ref::ID target_id) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::AddEntryPrinter{c, dir_id, name, target_id};

  } else {
    if (c->isOrphaned()) {
    _output.addEntry(c, dir_id, name, target_id);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.addEntry(source, c, dir_id, name, target_id);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::AddEntryPrinter{c, dir_id, name, target_id};
  }

  // Create an IR step and add it to the output trace
  _output.addEntry(source, c, dir_id, name, target_id);

  // Get the directory and target references
  auto dir = c->getRef(dir_id);
  auto target = c->getRef(target_id);

  // Did both references resolve?
  if (dir->isResolved() && target->isResolved()) {
    // Yes. Add the entry to the directory
    dir->getArtifact()->addEntry(c, name, target->getArtifact());

  } else {
    // No. Are we emulating or tracing?
    if (c->mustRun()) {
      // Tracing. Something bad has happened
      WARN << "Tried to add entry to directory with an invalid reference. dir=" << dir
           << ", target=" << target;

    } else {
      // Emulating. Report a change
      c->observeChange(Scenario::Build);
    }
  }
}

// Command c removes an entry from a directory
void Build::removeEntry(const IRSource& source,
                        const shared_ptr<Command>& c,
                        Ref::ID dir_id,
                        string name,
                        Ref::ID target_id) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::RemoveEntryPrinter{c, dir_id, name, target_id};

  } else {
    if (c->isOrphaned()) {
    _output.removeEntry(c, dir_id, name, target_id);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.removeEntry(source, c, dir_id, name, target_id);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::RemoveEntryPrinter{c, dir_id, name, target_id};
  }

  // Create an IR step and add it to the output trace
  _output.removeEntry(source, c, dir_id, name, target_id);

  // Get the directory and target references
  auto dir = c->getRef(dir_id);
  auto target = c->getRef(target_id);

  // Did both references resolve?
  if (dir->isResolved() && target->isResolved()) {
    // Yes. Remove the entry from the directory
    dir->getArtifact()->removeEntry(c, name, target->getArtifact());

  } else {
    // No. Are we emulating or tracing?
    if (c->mustRun()) {
      // Tracing. Something bad has happened
      WARN << "Tried to remove entry from directory with an invalid reference. dir=" << dir
           << ", target=" << target;

    } else {
      // Emulating. Report a change
      c->observeChange(Scenario::Build);
    }
  }
}

// A parent command launches a child command
void Build::launch(const IRSource& source,
                   const shared_ptr<Command>& parent,
                   const shared_ptr<Command>& child,
                   list<tuple<Ref::ID, Ref::ID>> refs) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (parent->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (parent->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::LaunchPrinter{parent, child, refs};

  } else {
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!parent->isLaunched()) {
      _deferred_commands.emplace(parent);
      _deferred_steps.launch(source, parent, child, refs);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::LaunchPrinter{parent, child, refs};
  }

  // Assign references in the child command
  for (const auto& [parent_ref_id, child_ref_id] : refs) {
    child->setRef(child_ref_id, parent->getRef(parent_ref_id));
  }

  // Add the child to the parent's list of children
  parent->addChild(child, refs);

  // Is the child going to run?
  if (child->mustRun()) {
    // Yes. The child is going to run

    // Mark it as executed
    child->setExecuted();

    // Create launch dependencies for the child
    child->createLaunchDependencies();

    // Update stats
    stats::traced_commands++;
    LOG(exec) << parent << " is launching " << child << " as a traced command";

  } else {
    // No. Update stats
    stats::emulated_commands++;
    LOG(exec) << parent << " is launching " << child << " as an emulated command";

    // If the child hasn't executed yet, report a change so it runs in the next phase
    if (!child->hasExecuted()) {
      // The command is not running now, and has never run before. Ensure it is marked.
      LOGF(rebuild, "{} changed: never run", child);
      child->observeChange(Scenario::Both);
    }
  }

  // Create an IR step and add it to the output trace
  _output.launch(source, parent, child, refs);

  

  // Is the parent command being emulated?
  if (parent->canEmulate()) {
    // Yes. We need to launch the child if it is supposed to run
    if (child->mustRun()) {
      // Start the child command in the tracer and record it as launched
      child->setLaunched(_tracer.start(*this, child));

    } else {
      // The child command is launched, and has no associated process
      child->setLaunched();
    }
  }

  // Print the command if requested
  if (child->mustRun() && options::print_on_run) {
    if (options::print_full) {
      _print_to << child->getFullName() << endl;
    } else {
      _print_to << child->getShortName(options::command_length) << endl;
    }
  }
}

// A parent command displays its orphans
void Build::orphan(const shared_ptr<Command>& parent,
                   const shared_ptr<Command>& child,
                   list<tuple<Ref::ID, Ref::ID>> refs) noexcept {
  _deferred_commands.emplace(child);
  if(!parent->mustRun()){
    child->setOrphaned();
  }
  _output.orphan(parent, child, refs);
  cout << "in orphan" << "\n";
  cout << "parent: " << parent << "  orphan: " << child << "\n";
}

// Command c waits for a child command to exit
void Build::join(const IRSource& source,
                 const shared_ptr<Command>& c,
                 const shared_ptr<Command>& child,
                 int exit_status) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(ir) << "traced " << TracePrinter::JoinPrinter{c, child, exit_status};

  } else {
    if (c->isOrphaned()) {
    _output.join(c, child, exit_status);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      _deferred_commands.emplace(c);
      _deferred_steps.join(source, c, child, exit_status);
      return;
    }

    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::JoinPrinter{c, child, exit_status};
  }

  // If we're emulating the parent command but the child is running, wait for it now
  if (c->canEmulate() && child->mustRun()) {
    // If the child command is running in the tracer, wait for it
    const auto& process = child->getProcess();
    if (process) _tracer.wait(*this, process);
  }

  // Create an IR step and add it to the output trace
  _output.join(source, c, child, exit_status);

  // If the parent is emulated, check for the expected exit status
  if (c->canEmulate() && child->getExitStatus() != exit_status) {
    LOGF(rebuild, "{} changed: child {} exited with different status (expected {}, observed {})", c,
         child, exit_status, child->getExitStatus());

    // The command detects a changed exit status from its child, so it must rerun
    c->observeChange(Scenario::Both);
  }
}

// Command c is exiting
void Build::exit(const IRSource& source, const shared_ptr<Command>& c, int exit_status) noexcept {
  // If the command must run but the step comes from a saved source, skip it
  if (c->mustRun() && !source.isExecuting()) return;

  // Is this step from a traced command?
  if (c->mustRun()) {
    stats::traced_steps++;
    // Log the traced step
    LOG(exec) << c << " traced exits.";;
    LOG(ir) << "traced " << TracePrinter::ExitPrinter{c, exit_status};

  } else {
    if (c->isOrphaned()) {
     LOG(exec) << c << " orphaned exits.";;
    _output.exit(c, exit_status);
    return;
  }
    // If this step comes from a command that hasn't been launched, we need to defer this step
    if (!c->isLaunched()) {
      LOG(exec) << c << " deferred exits.";
      _deferred_commands.emplace(c);
      _deferred_steps.exit(source, c, exit_status);
      return;
    }
    LOG(exec) << c << " emulated exits.";
    stats::emulated_steps++;
    // Log the emulated step
    LOG(ir) << "emulated " << TracePrinter::ExitPrinter{c, exit_status};
  }
  
  auto it = c->getRefLists().begin();
  if(c->mustRun()){
    runDeferredSteps();
  for(const auto& candidate : c->getMaxChildren()){
    if(std::find(c->getTempChildren().begin(), c->getTempChildren().end(), candidate) == c->getTempChildren().end()){
    cout << "in exit" << "\n";
    candidate->setOrphaned();
    _output.orphan(c, candidate, *it);
    runDeferredSteps();
    }
    std::advance(it, 1);
  }
  }
  


  // Create an IR step and add it to the output trace
  _output.exit(source, c, exit_status);

  // Save the exit status for this command
  c->setExitStatus(exit_status);

  // Is this emulated command running in a process? If so, we need to let it exit
  const auto& process = c->getProcess();
  if (c->canEmulate() && process) {
    process->forceExit(exit_status);
  }

  // Cache and fingerprint everything in the environment at the end of this command's run
  if (c->mustRun()) env::cacheAll();

}

// Look for a known command that matches one being launched
shared_ptr<Command> Build::findCommand(const shared_ptr<Command>& parent,
                                       vector<string> args,
                                       const map<int, Ref::ID>& fds) noexcept {
  // Keep track of the best matching command so far
  shared_ptr<Command> child = nullptr;

  // Matches may require path substitutions for temporary files. Remember those.
  map<string, string> child_substitutions;
  
  // TODO: Should tempfile substitutions be global? Probably. For now they are unique to each
  // command, which could cause problems in strange cases.

for (const auto& candidate : _deferred_commands) {
  cout<<"FINDCOMMAND--------deferred_command: " << candidate << "\n";
}
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
  // Did we find a matching command?
  if (child) {
    // Remove the child from the deferred command set
    _deferred_commands.erase(child);
    // We found a matching child command. Apply the required substitutions
    child->applySubstitutions(child_substitutions);

    cout << "try to match parent: " << parent  << "   matched child: " << child << "\n";
    LOG(exec) << "Matched launch of " << child << " by " << parent;

  } else {
    
    // Create a child and mark it as running
    child = make_shared<Command>(args);
    child->setMarking(RebuildMarking::MustRun);

    LOG(exec) << "New command " << child << " did not match any previous command.";
    // Set initial file descriptors for the new child
    // Set the reference for each FD using the parent's references
    for (const auto& [fd, parent_ref] : fds) {
      auto child_ref = child->nextRef();
      child->addInitialFD(fd, child_ref);
    }
  }
  
  return child;
}
