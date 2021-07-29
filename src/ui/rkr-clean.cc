#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
#include "artifacts/DirArtifact.hh"
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
using std::nullopt;
using std::static_pointer_cast;
using std::string;
using std::vector;

/**
 * Run the 'clean' subcommand
 */
void do_clean(vector<string> args, bool clean_all) noexcept {
  string cmd;
  if (clean_all) {
    options::track_inputs_outputs = true;
    // Load the serialized build trace
    auto [root_cmd, trace] = InputTrace::load(constants::DatabaseFilename, args);

    // Emulate the trace
    trace->sendTo(Build());

    for (const auto& weak_artifact : env::getArtifacts()) {
      auto a = weak_artifact.lock();
      if (a->getTypeName() != "Dir") continue;

      // if (a->getName().empty()) {
      //   cout << "  " << a->getTypeName() << ": <anonymous>" << endl;
      // } else {
      //   cout << "  " << a->getTypeName() << ": " << a->getName() << endl;
      // }

      // size_t index = 0;
      // for (const auto& v : a->getVersions()) {
      //   cout << "    v" << index << ": " << v << endl;
      //   index++;
      // }
      // cout << endl;
      auto dir = static_pointer_cast<DirArtifact>(a);
      auto entries = dir->peekEntries();
      for (const auto& entry : entries) {
        if (entry.second->getNewlyCreated()) {
          cout << "Removing " << entry.first << endl;
          auto path = entry.second->peekTarget()->getPath();
          if (path != nullopt) {
            cmd = "rm " + path.value().string();
            // cout << cmd << endl;
            system(cmd.c_str());
          }
        }
      }
    }
  } else {
    cmd = "rm -rf .rkr";
    system(cmd.c_str());
    cout << ".rkr removed" << endl;
  }
}