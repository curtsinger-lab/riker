#include <algorithm>
#include <array>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "artifacts/Artifact.hh"
#include "data/InputTrace.hh"
#include "runtime/Build.hh"
#include "runtime/env.hh"
#include "util/constants.hh"

using std::array;
using std::cout;
using std::endl;
using std::ifstream;
using std::ofstream;
using std::string;
using std::unique_ptr;
using std::vector;

/**
 * Run the 'gen_deps' subcommand
 */
void do_gen_deps(vector<string> args) noexcept {
  // Turn on input/output tracking
  options::track_inputs_outputs = true;

  // Load the serialized build trace
  auto [root_cmd, trace] = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the trace
  trace->sendTo(Build());

  ofstream myfile;
  myfile.open(".rkr-deps");

  // list of packages needed
  vector<string> packages;

  // Iterate through each artifact
  for (const auto& weak_artifact : env::getArtifacts()) {
    auto a = weak_artifact.lock();

    // If a is a null pointer, then skip
    if (!a) continue;
    // If a is an anonymous artifact, skip
    if (a->getName().empty()) continue;
    // If a is a directory, skip
    if (a->getTypeName() == "Dir") continue;
    // If a is special, skip
    if (a->getTypeName() == "Special") continue;
    // Remove all files in source folder
    if (a->getName().find("src") != string::npos) continue;
    // Remove Riker related files
    if (a->getName().find("riker") != string::npos) continue;
    // Remove Rikerfile
    if (a->getName() == "Rikerfile") continue;
    // Remove Symlink
    if (a->getTypeName() == "Symlink") continue;
    // Remove ld.so.cache
    if (a->getName().find("ld.so.cache") != string::npos) continue;
    // Remove locale-archive
    if (a->getName().find("locale-archive") != string::npos) continue;
    // Remove proc/filesystems
    if (a->getName().find("proc/filesystems") != string::npos) continue;

    string path = a->getCommittedPath().value().string();
    if (path == "/usr/bin/ls") {
      path.erase(0, 4);
    } else if (path.find("include") == string::npos) {
      if (path.find("/usr/") != string::npos) path.erase(0, 4);
      path = "'*" + path + "'";
    }
    // myfile << a->getTypeName() << ": " << path << endl;
    string command = "dpkg -S " + path + " 2>&1";
    FILE* pipe = popen(command.c_str(), "r");
    if (!pipe) {
      exit(EXIT_FAILURE);
    }
    array<char, 128> buffer;
    string result;
    while (fgets(buffer.data(), buffer.size(), pipe) != NULL) {
      result += buffer.data();
    }
    string package = result.substr(0, result.find(" "));
    package.pop_back();
    if (find(packages.begin(), packages.end(), package) == packages.end()) {
      packages.push_back(package);
      myfile << package << endl;
    }
    pclose(pipe);
  }
  myfile.close();
}

void do_install_deps(vector<string> args) noexcept {
  array<char, 128> buffer;
  string result, package;
  ifstream myfile(".rkr-deps");
  if (myfile.is_open()) {
    while (getline(myfile, package)) {
      string cmd = "dpkg-query -W " + package;
      unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd.c_str(), "r"), pclose);
      if (!pipe) exit(EXIT_FAILURE);
      while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        result += buffer.data();
      }
      if (result.find("no packages found matching") != string::npos) {
        cout << "Installing" << package << endl;
        cmd = "dpkg -i " + package;
        system(cmd.c_str());
      } else {
        cout << package << " is already installed" << endl;
      }
    }
  }
  myfile.close();
}

void do_check_deps(vector<string> args) noexcept {
  ifstream f(".rkr-deps");
  if (f.is_open()) {
    cout << f.rdbuf();
  }
}