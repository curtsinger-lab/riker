#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "data/InputTrace.hh"
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
void do_trace(vector<string> args, string output) noexcept {
  auto trace = InputTrace::load(constants::DatabaseFilename, args);
  FAIL_IF(!trace) << "No trace available. Run a build first.";

  // Are we printing to stdout or a file?
  if (output == "-") {
    trace->sendTo(TracePrinter(cout));
  } else {
    trace->sendTo(TracePrinter(ofstream(output)));
  }
}
