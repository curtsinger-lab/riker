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
 * \param input     The name of the trace file to display
 * \param output    The name of the output file, or "-" for stdout
 */
void do_trace(vector<string> args, string input, string output) noexcept {
  auto trace = TraceReader::load(input);
  FAIL_IF(!trace) << "A trace could not be loaded. Run a full build first.";

  // Are we printing to stdout or a file?
  if (output == "-") {
    trace->sendTo(TracePrinter(cout));
  } else {
    trace->sendTo(TracePrinter(ofstream(output)));
  }
}
