#include <iostream>
#include <string>
#include <vector>

#include "data/InputTrace.hh"
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
  auto trace = InputTrace::load(constants::DatabaseFilename, args);
  FAIL_IF(!trace) << "No trace to load. You need to run a build to produce a graph.";
  auto root_cmd = trace->getRootCommand();

  // Emulate the loaded trace
  Build eval;
  trace->sendTo(eval);

  // Plan the next build
  root_cmd->planBuild();

  // Print commands that must run
  bool must_run_header_printed = false;
  for (const auto& c : root_cmd->collectMustRun()) {
    // Print the header if necessary
    if (!must_run_header_printed) {
      cout << "Commands that must run:" << endl;
      must_run_header_printed = true;
    }

    // Print the command
    cout << "  " << c->getShortName(options::command_length) << endl;
  }

  // If we printed anything, add a newline
  if (must_run_header_printed) cout << endl;

  // Print the rebuild plan
  bool may_run_header_printed = false;
  for (const auto& c : root_cmd->collectMayRun()) {
    if (!may_run_header_printed) {
      cout << "Commands that may run:" << endl;
      may_run_header_printed = true;
    }
    cout << "  " << c->getShortName(options::command_length) << endl;
  }

  // If we never printed the header, there were no commands to rerun
  if (!must_run_header_printed && !may_run_header_printed) {
    cout << "No commands to rerun" << endl;
  }
}
