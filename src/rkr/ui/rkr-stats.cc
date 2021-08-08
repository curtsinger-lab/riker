#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
#include "data/InputTrace.hh"
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
 * Run the `stats` subcommand
 * \param list_artifacts  Should the output include a list of artifacts and versions?
 */
void do_stats(vector<string> args, bool list_artifacts) noexcept {
  // Turn on input/output tracking
  options::track_inputs_outputs = true;

  // Reset the stats counters
  reset_stats();

  // Load the serialized build trace
  auto trace = InputTrace::load(constants::DatabaseFilename, args);
  FAIL_IF(!trace) << "No trace was loaded. Run a build first.";

  // Emulate the trace
  trace->sendTo(Build());

  // Print statistics
  cout << "Build Statistics:" << endl;
  cout << "  Commands: " << stats::emulated_commands << endl;
  cout << "  Steps: " << stats::emulated_steps << endl;
  cout << "  Artifacts: " << stats::artifacts << endl;
  cout << "  Artifact Versions: " << stats::versions << endl;

  if (list_artifacts) {
    cout << endl;
    cout << "Artifacts:" << endl;
    for (const auto& weak_artifact : env::getArtifacts()) {
      auto a = weak_artifact.lock();
      if (!a) continue;

      if (a->getName().empty()) {
        cout << "  " << a->getTypeName() << ": <anonymous>" << endl;
      } else {
        cout << "  " << a->getTypeName() << ": " << a->getName() << endl;
      }

      size_t index = 0;
      for (const auto& v : a->getVersions()) {
        cout << "    v" << index << ": " << v << endl;
        index++;
      }
      cout << endl;
    }
  }
}
