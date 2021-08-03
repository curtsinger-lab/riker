#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <sys/mman.h>

#include "artifacts/Artifact.hh"
#include "data/InputTrace.hh"
#include "data/NewOutputTrace.hh"
#include "data/OutputTrace.hh"
#include "data/ReadWriteCombiner.hh"
#include "runtime/Build.hh"
#include "runtime/env.hh"
#include "ui/commands.hh"
#include "util/Graph.hh"
#include "util/TracePrinter.hh"
#include "util/constants.hh"
#include "util/stats.hh"

using std::cout;
using std::endl;
using std::ofstream;
using std::string;
using std::stringstream;
using std::vector;

/**
 * Run the `bench` subcommand
 */
void do_bench(std::vector<std::string> args) noexcept {
  // Load the serialized build trace
  auto [root_cmd, trace] = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the trace
  trace->sendTo(NewOutputTrace("newdb"));
}
