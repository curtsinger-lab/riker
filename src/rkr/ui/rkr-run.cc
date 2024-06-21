#include <filesystem>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "data/DefaultTrace.hh"
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
 * Run the `run` subcommand.
 */
void do_run(vector<string> args,
            string command_output,
            string binary_output,
            optional<string> run_commands) noexcept {
  // Make sure the output directory exists
  fs::create_directories(constants::OutputDir);

  // Save the commands passed for riker to run
  vector<string> remaining;

  if (run_commands.has_value()) {
    remaining.push_back(run_commands.value());
  }

  // Also ensure that the cache directory exists
  fs::create_directory(constants::CacheDir);

  // Set up an ostream to print to if necessary
  unique_ptr<ostream> print_to;
  if (command_output != "-") {
    print_to = make_unique<ofstream>(command_output);
  }

  // Set up an ostream to print binary to if necessary
  unique_ptr<ostream> binary_to;
  if (binary_output != "-") {
    binary_to = make_unique<ofstream>(binary_output);
  }

  // The input TraceReader will supply the trace to each phase except the first
  TraceReader input;

  // Keep track of the root command
  shared_ptr<Command> root_cmd;

  LOG(phase) << "Starting run phase 0";

  // No trace was loaded. Set up a default trace
  DefaultTrace def(remaining);

  // Remember the root command
  root_cmd = def.getRootCommand();

  // Create a trace writer to store the output trace
  TraceWriter output;

  // Evaluate the default trace
  Build eval(output, print_to ? *print_to : std::cout);
  def.sendToHelp(eval, remaining[0]);

  // The output now holds the next input. Save it
  input = output.getReader();

  // Plan the next phase of the build
  root_cmd->planBuild();

  LOG(phase) << "Finished build phase 0";

  // Keep track of the build phase
  size_t iteration = 1;

  // Loop as long as there are commands left to run
  while (!root_cmd->allFinished()) {
    // Prepare a new output buffer with read/write combining
    auto output = ReadWriteCombiner<TraceWriter>();

    // Revert the environment to committed state
    env::rollback();

    LOGF(phase, "Starting run phase {}", iteration);

    // Run the trace and send the new trace to output
    Build build(output, print_to ? *print_to : std::cout);
    input.sendTo(build);

    // Plan the next iteration
    root_cmd->planBuild();

    LOGF(phase, "Finished run phase {}", iteration);

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

  // Print binary trace to file if requested
  if (binary_output != "-") {
    std::filesystem::copy(constants::DatabaseFilename, binary_output,
                          std::filesystem::copy_options::overwrite_existing);
  }

  if (options::syscall_stats) {
    Tracer::printSyscallStats();
  }
}