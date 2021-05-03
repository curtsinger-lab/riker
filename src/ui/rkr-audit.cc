#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "data/DefaultTrace.hh"
#include "data/IRBuffer.hh"
#include "data/OutputTrace.hh"
#include "data/PostBuildChecker.hh"
#include "data/ReadWriteCombiner.hh"
#include "runtime/Build.hh"
#include "runtime/env.hh"
#include "ui/commands.hh"
#include "util/constants.hh"
#include "util/stats.hh"

namespace fs = std::filesystem;

using std::make_shared;
using std::ofstream;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;

/**
 * Run the `audit` subcommand.
 */
void do_audit(vector<string> args, string command_output) noexcept {
  // Turn on printing, since that's the point of this mode
  options::print_on_run = true;
  options::print_full = true;

  // Set up an ostream to print to if necessary
  shared_ptr<ostream> print_to;
  if (command_output != "-") {
    print_to = make_shared<ofstream>(command_output);
  }

  // Set up a default trace
  DefaultTrace input(args);
  auto root_cmd = input.getRootCommand();

  // Set up an IRBuffer to hold the output
  IRBuffer output;

  // Run the trace and send the new trace to output
  Build phase1(output, print_to);
  input.sendTo(phase1);

  // Plan a rebuild so the build actually executes
  root_cmd->planBuild();

  // Now run the actual build. The run depends on whether we're printing to cout or a file
  Build phase2(print_to);
  output.sendTo(phase2);
}
