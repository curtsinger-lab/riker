#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "data/Trace.hh"
#include "ui/commands.hh"
#include "util/TracePrinter.hh"
#include "util/constants.hh"

using std::cout;
using std::ofstream;
using std::string;
using std::vector;

/**
 * Run the `trace` subcommand
 * \param output    The name of the output file, or "-" for stdout
 */
void do_trace(string output, string trace_binary, string trace_read) noexcept {
  fs::path pathname;

  // Check if a path was passed to read from, otherwise, read from current directory.
  if (trace_read != "-") {
    pathname = trace_read;
  } else {
    pathname = constants::DatabaseFilename;
  }

  auto trace = TraceReader::load(pathname);

  FAIL_IF(!trace) << "A trace could not be loaded. Run a full build first.";

  // Print binary trace to file if requested
  if (trace_binary != "-") {
    // const auto copyOptions = fs::copy_options::overwrite_existing;
    std::filesystem::copy(constants::DatabaseFilename, trace_binary,
                          std::filesystem::copy_options::overwrite_existing);
  }

  // Are we printing to stdout or a file?
  if (output == "-") {
    trace->sendTo(TracePrinter(cout));
  } else {
    trace->sendTo(TracePrinter(ofstream(output)));
  }
}
