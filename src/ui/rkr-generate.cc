#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <vector>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

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
using std::fstream;
using std::shared_ptr;
using std::string;
using std::vector;

string rikerfile_name = "Rikerfile-gen";

// Helper function for converting to execv format
char* convert_helper(const string& s) {
  char* pc = new char[s.size() + 1];
  strcpy(pc, s.c_str());
  return pc;
}

// Function to check if a String ends in another string.
bool hasEnding(string const& fullString, string const& ending) {
  if (fullString.length() >= ending.length()) {
    return (0 ==
            fullString.compare(fullString.length() - ending.length(), ending.length(), ending));
  } else {
    return false;
  }
}

// Function removes all lines whose first token ends in ':', filtering out non-command messages
void removeComments(string temp_name = "temp.txt") {
  fstream original_file(rikerfile_name.c_str(), fstream::in);
  fstream new_file(temp_name, fstream::trunc | fstream::out);
  string line, token, delimiter = " ";
  while (getline(original_file, line)) {
    token = line.substr(0, line.find(delimiter));
    if (!hasEnding(token, ":")) {
      new_file << line << endl;
    }
  }
  remove(rikerfile_name.c_str());
  rename(temp_name.c_str(), rikerfile_name.c_str());
}

void getCommandArgs(vector<vector<string>>* command_args) {
  fstream original_file(rikerfile_name.c_str(), fstream::in);
  string line, token, delimiter = " ";

  // Skips the first line (#!/bin/sh)
  getline(original_file, line);

  // Iterate over every line in the Rikerfile
  while (getline(original_file, line)) {
    size_t pos = 0;
    string line_copy(line);
    while ((pos = line_copy.find(delimiter)) != string::npos) {
    }
    token = line.substr(0, pos);
    if (token != "") {
    }
  }
  original_file.close();
}

void executeCommand(string path, vector<string> args, bool has_own_thread = true) {
  vector<char*> vc;
  transform(args.begin(), args.end(), back_inserter(vc), convert_helper);
  vc.push_back(NULL);

  if (has_own_thread) {
    int stat = 0;
    if (fork() == 0) {
      execv(path.c_str(), vc.data());
    } else {
      wait(&stat);
    }
    if (WIFSIGNALED(stat)) {
      psignal(WTERMSIG(stat), "Exit signal");
      exit(0);
    }
  } else {
    execv(path.c_str(), vc.data());
  }
}

// Functions dictating the 3 main steps for Rikerfile generation (and the main function):
//   1. Create a new Rikerfile with #!/bin/sh header and pipe make commands to it
//   2. Process the make commands by removing unnecessary lines and simplifying commands
//   3. Finalize the Rikerfile by making it executable
//   4. Main function to call these helper functions in order

void createRikerfile() {
  // Create empty Rikerfile
  fstream write_file(rikerfile_name.c_str(), fstream::trunc | fstream::out);

  // Write shell header to Rikerfile
  write_file << "#!/bin/sh" << endl << endl;
  write_file.close();

  // Pipe make commands into Rikerfile
  string command = "make -n --always-make >> " + rikerfile_name;
  pclose(popen(command.c_str(), "w"));
}

void processRikerfile() {
  // Filter out non-command messages
  removeComments();

  vector<vector<string>> command_args;
  // getCommandArgs(&command_args);
}

void finalizeRikerfile() {
  // Make the Rikerfile executable by calling "chmod u+x Rikerfile".
  vector<string> vs = {"chmod", "u+x", rikerfile_name};
  executeCommand("chmod", vs);
}

void printTree(shared_ptr<Command> root_cmd) {
  if (!root_cmd->isEmptyCommand()) {
    cout << root_cmd->getFullName() << endl;
  }

  // If root_cmd arg 0 is "make" (or path to make), store children in list of commands for rikerfile
  for (auto child : root_cmd->getChildren()) {
    printTree(child);
  }
}

void do_generate(vector<string> args) noexcept {
  // Turn on input/output tracking
  options::track_inputs_outputs = true;

  // Reset the stats counters
  reset_stats();

  // Load the serialized build trace
  auto [root_cmd, trace] = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the trace
  trace->sendTo(Build());

  printTree(root_cmd);

  // createRikerfile();

  // processRikerfile();

  // finalizeRikerfile();
}