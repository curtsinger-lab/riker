#include <filesystem>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

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

  // Load the input trace from the database (or use a default if no trace exists)
  // This returns a root command and IRSource
  auto [root_cmd, input] = InputTrace::load(constants::DatabaseFilename, args);

  // Output from this phase goes directly to an IRBuffer
  auto output = make_unique<IRBuffer>();

  // Count build iterations
  size_t iteration = 0;

  // Start the build loop
  do {
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

    // The output becomes the new input
    input = std::move(output);

    // Prepare a new output buffer with read/write combining
    output = make_unique<ReadWriteCombiner<IRBuffer>>();

    // Write stats out to CSV & reset counters
    gather_stats(stats_log_path, stats, iteration);
    reset_stats();

    // Increment the iteration
    iteration++;

    // Keep looping as long as there are commands to run
  } while (!root_cmd->allFinished());

  // Commit anything left in the environment
  LOG(phase) << "Committing environment changes";
  env::commitAll();

  // If more than one phase of the build ran, then we know the trace could have changed
  if (iteration > 1) {
    LOG(phase) << "Starting post-build checks";

    // Run the post-build checks and send the resulting trace directly to output
    PostBuildChecker<OutputTrace> output(constants::DatabaseFilename);

    // Reset the environment
    env::rollback();

    // Run the build
    Build build(output, print_to ? *print_to : std::cout);
    input->sendTo(build);

    LOG(phase) << "Finished post-build checks";
  }

  gather_stats(stats_log_path, stats, iteration);
  write_stats(stats_log_path, stats);

  if (options::syscall_stats) {
    Tracer::printSyscallStats();
  }
}
