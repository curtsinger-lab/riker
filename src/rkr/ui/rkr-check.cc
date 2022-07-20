#include <iostream>
#include <string>
#include <vector>

#include "data/Trace.hh"
#include "runtime/Build.hh"
#include "ui/commands.hh"
#include "util/constants.hh"
#include "util/options.hh"

using std::cout;
using std::endl;
using std::string;
using std::vector;

/**
 * Run the `check` subcommand
 */
void do_check(vector<string> args) noexcept {
  // Load the build trace
  auto trace = TraceReader::load(constants::DatabaseFilename);
  FAIL_IF(!trace) << "A trace could not be loaded. Run a full build first.";
  auto root_cmd = trace->getRootCommand();

  // Emulate the loaded trace
  Build eval;
  trace->sendTo(eval);

  // Plan the next build
  root_cmd->planBuild();

  // Get the set of commands that must run
  auto must_run = root_cmd->collectMustRun();

  if (must_run.size() > 0) {
    cout << "Commands that must run:" << endl;
    for (const auto& c : must_run) {
      cout << "  " << c->getShortName(options::command_length) << endl;
    }

  } else {
    cout << "No commands to rerun" << endl;
  }
}
