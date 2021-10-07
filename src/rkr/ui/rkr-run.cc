#include <filesystem>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "data/Trace.hh"
#include "runtime/Build.hh"
#include "runtime/env.hh"
#include "ui/commands.hh"

namespace fs = std::filesystem;

using std::make_shared;
using std::string;

/**
 * Run the `run` subcommand.
 */
void do_run(string input_file) noexcept {
  // Create a TraceReader to read the trace
  auto in = TraceReader::load(input_file);

  // Did we fail to load a file?
  if (!in.has_value()) {
    std::cerr << "Trace file " << input_file << " could not be loaded.\n";
    exit(EXIT_FAILURE);
  }

  // Create a Build to evaluate the trace
  Build evaluator;

  // Run the trace through the evaluator
  in->sendTo(evaluator);

  // Now commit any changes in the environment
  env::commitAll();
}
