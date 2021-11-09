#include <filesystem>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "data/DefaultTrace.hh"
#include "data/EmulateOnly.hh"
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

  // The input TraceReader will supply the trace to each phase except the first
  TraceReader input;

  // Keep track of the root command
  shared_ptr<Command> root_cmd;

  LOG(phase) << "Starting build phase 0";

  // Is there a trace to load?
  if (auto loaded = TraceReader::load(constants::DatabaseFilename); loaded) {
    // Yes. Remember the root command
    root_cmd = loaded->getRootCommand();

    // Create a trace writer to store the output trace
    TraceWriter output;

    // Evaluate the loaded trace
    Build eval(output, print_to ? *print_to : std::cout);
    loaded->sendTo(eval);

    // The output now holds the next input. Save it
    input = output.getReader();

  } else {
    // No trace was loaded. Set up a default trace
    DefaultTrace def(args);

    // Remember the root command
    root_cmd = def.getRootCommand();

    // Create a trace writer to store the output trace
    TraceWriter output;

    // Evaluate the default trace
    Build eval(output, print_to ? *print_to : std::cout);
    def.sendTo(eval);

    // The output now holds the next input. Save it
    input = output.getReader();
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
    // Prepare a new output buffer with read/write combining
    auto output = ReadWriteCombiner<TraceWriter>();

    // Revert the environment to committed state
    env::rollback();

    LOGF(phase, "Starting build phase {}", iteration);

    // Run the trace and send the new trace to output
    Build build(output, print_to ? *print_to : std::cout);
    EmulateOnly filter(build);
    input.sendTo(filter);

    // Plan the next iteration
    root_cmd->planBuild();

    //LOGF(phase, "Before finishing phase, deferred commands: ", _deferred_commands.size());

    LOGF(phase, "Finished build phase {}", iteration);

    // Write stats out to CSV & reset counters
    gather_stats(stats_log_path, stats, iteration);
    reset_stats();

    // Increment the iteration
    iteration++;

    // The output becomes the next input
    input = output.getReader();
  }

  // Commit anything left in the environment
  LOG(phase) << "Committing environment changes";
  env::commitAll();

  // If more than one phase of the build ran, then we know the trace could have changed
  if (iteration > 1) {
    LOG(phase) << "Starting post-build checks";

    // Run the post-build checks and send the resulting trace directly to output
    PostBuildChecker<TraceWriter> output(constants::DatabaseFilename);

    // Reset the environment
    env::rollback();

    // Evaluate the trace in the output buffer from the last phase
    Build build(output, print_to ? *print_to : std::cout);
    input.sendTo(build);

    LOG(phase) << "Finished post-build checks";
  }

  gather_stats(stats_log_path, stats, iteration);
  write_stats(stats_log_path, stats);

  if (options::syscall_stats) {
    Tracer::printSyscallStats();
  }
}
