#include <filesystem>
#include <optional>
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

using std::make_unique;
using std::string;
using std::unique_ptr;
using std::vector;

/**
 * Run the `audit` subcommand.
 */
void do_audit(vector<string> args) noexcept {
  // Turn on printing, since that's the point of this mode
  options::print_on_run = true;
  options::print_full = true;

  // Set up a default trace
  DefaultTrace input(args);
  auto root_cmd = input.getRootCommand();

  // Set up an IRBuffer to hold the output
  IRBuffer output;

  // Run the trace and send the new trace to output
  Build phase1(output);
  input.sendTo(phase1);

  // Plan a rebuild so the build actually executes
  root_cmd->planBuild();

  // Now run the actual build
  Build phase2;
  output.sendTo(phase2);
}
