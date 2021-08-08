#include <filesystem>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "data/DefaultTrace.hh"
#include "data/EmulateOnly.hh"
#include "data/IRBuffer.hh"
#include "data/InputTrace.hh"
#include "data/OutputTrace.hh"
#include "data/PostBuildChecker.hh"
#include "data/ReadWriteCombiner.hh"
#include "data/Trace.hh"
#include "runtime/Build.hh"
#include "runtime/env.hh"
#include "tracing/Tracer.hh"
#include "ui/commands.hh"
#include "util/constants.hh"
#include "util/stats.hh"

namespace fs = std::filesystem;

using std::make_unique;
using std::ofstream;
using std::optional;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::vector;

/**
 * Run the `build` subcommand.
 */
void do_build(vector<string> args,
              optional<fs::path> stats_log_path,
              string command_output) noexcept {
  // Make sure the output directory exists
  fs::create_directories(constants::OutputDir);

  // Also ensure that the cache directory exists
  fs::create_directory(constants::CacheDir);

  // Set up an ostream to print to if necessary
  unique_ptr<ostream> print_to;
  if (command_output != "-") {
    print_to = make_unique<ofstream>(command_output);
  }

  // Build stats
  optional<string> stats;

  // Reset the statistics counters
  reset_stats();

  // The output from the first phase will go to an IRBuffer
  auto output = make_unique<IRBuffer>();

  // Keep track of the root command
  shared_ptr<Command> root_cmd;

  LOG(phase) << "Starting build phase 0";

  // Is there a trace to load?
  if (auto loaded = InputTrace::load(constants::DatabaseFilename, args); loaded) {
    // Yes. Remember the root command
    root_cmd = loaded->getRootCommand();

    // Evaluate the loaded trace
    Build eval(*output, print_to ? *print_to : std::cout);
    loaded->sendTo(eval);

  } else {
    // No trace was loaded. Set up a default trace
    DefaultTrace def(args);

    // Remember the root command
    root_cmd = def.getRootCommand();

    // Evaluate the default trace
    Build eval(*output, print_to ? *print_to : std::cout);
    def.sendTo(eval);
  }

  // Plan the next phase of the build
  root_cmd->planBuild();

  LOG(phase) << "Finished build phase 0";

  // Write stats out to CSV & reset counters
  gather_stats(stats_log_path, stats, 0);
  reset_stats();

  // Keep track of the build phase
  size_t iteration = 1;

  // Loop as long as there are commands left to run
  while (!root_cmd->allFinished()) {
    // The output from the previous phase becomes the new input
    auto input = std::move(output);

    // Prepare a new output buffer with read/write combining
    output = make_unique<ReadWriteCombiner<IRBuffer>>();

    // Revert the environment to committed state
    env::rollback();

    LOGF(phase, "Starting build phase {}", iteration);

    // Run the trace and send the new trace to output
    Build build(*output, print_to ? *print_to : std::cout);
    EmulateOnly filter(build);
    input->sendTo(filter);

    // Plan the next iteration
    root_cmd->planBuild();

    LOGF(phase, "Finished build phase {}", iteration);

    // Write stats out to CSV & reset counters
    gather_stats(stats_log_path, stats, iteration);
    reset_stats();

    // Increment the iteration
    iteration++;
  }

  // Commit anything left in the environment
  LOG(phase) << "Committing environment changes";
  env::commitAll();

  // If more than one phase of the build ran, then we know the trace could have changed
  if (iteration > 1) {
    LOG(phase) << "Starting post-build checks";

    // Run the post-build checks and send the resulting trace directly to output
    PostBuildChecker<OutputTrace> post(constants::DatabaseFilename);

    // Reset the environment
    env::rollback();

    // Evaluate the trace in the output buffer from the last phase
    Build build(post, print_to ? *print_to : std::cout);
    output->sendTo(build);

    LOG(phase) << "Finished post-build checks";
  }

  gather_stats(stats_log_path, stats, iteration);
  write_stats(stats_log_path, stats);

  if (options::syscall_stats) {
    Tracer::printSyscallStats();
  }
}
