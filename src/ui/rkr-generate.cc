#include <algorithm>
#include <bitset>
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

using std::bitset;
using std::cout;
using std::endl;
using std::fstream;
using std::remove;
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

// // Function removes all lines whose first token ends in ':', filtering out non-command messages
// void removeComments(string temp_name = "temp.txt") {
//   fstream original_file(rikerfile_name.c_str(), fstream::in);
//   fstream new_file(temp_name, fstream::trunc | fstream::out);
//   string line, token, delimiter = " ";
//   while (getline(original_file, line)) {
//     token = line.substr(0, line.find(delimiter));
//     if (!hasEnding(token, ":")) {
//       new_file << line << endl;
//     }a
//   }
//   remove(rikerfile_name.c_str());
//   rename(temp_name.c_str(), rikerfile_name.c_str());
// }

// Erase all Occurrences of to_find with to_replace
void replaceToken(string& str, const string& to_find, const string& to_replace) {
  size_t pos = 0;
  while ((pos = str.find(to_find, pos)) != string::npos) {
    str.replace(pos, to_find.length(), to_replace);
    pos += to_replace.length();
  }
}

// Function formats shell commands by removing new line characters and encapsulating 3rd argument in
// quotes.
void formatShellCommands(string temp_name = "temp.txt") {
  fstream original_file(rikerfile_name.c_str(), fstream::in);
  fstream new_file(temp_name, fstream::trunc | fstream::out);
  string line, token, delimiter = " ";
  while (getline(original_file, line)) {
    token = line.substr(0, line.find(delimiter));
    if (token.compare("/bin/sh") == 0) {
      string line_copy(line);

      // Delete all new line characters
      replaceToken(line_copy, "\\\\n", "");

      // Add backslash to all existing quotation marks
      replaceToken(line_copy, "\"", "\\\"");

      // Encapsulate 3rd argument with quotations
      replaceToken(line_copy, "-c ", "-c \"");
      line_copy += "\"";

      new_file << line_copy << endl;
    } else {
      new_file << line << endl;
    }
  }
  remove(rikerfile_name.c_str());
  rename(temp_name.c_str(), rikerfile_name.c_str());
}

void getCommandArgs(vector<vector<string>>& command_args) {
  fstream original_file(rikerfile_name.c_str(), fstream::in);
  string line, token, delimiter = " ";
  // size_t delim_length = delimiter.length();
  getline(original_file, line);

  // Iterate over every line in the Rikerfile
  while (getline(original_file, line)) {
    replaceToken(line, "\\\\n", "");
    // size_t pos_start = 0, pos_end;
    // vector<string> line_args;
    // while ((pos_end = line.find(delimiter, pos_start) != string::npos)) {
    //   token = line.substr(pos_start, pos_end - pos_start);
    //   cout << token << endl;
    //   pos_start = pos_end + delim_length;
    //   line_args.push_back(token);
    // }
    vector<string> list;
    string s = line;
    size_t pos = 0;
    string token;
    while ((pos = s.find(delimiter)) != string::npos) {
      token = s.substr(0, pos);
      replaceToken(token, " ", "");
      list.push_back(token);
      s.erase(0, pos + delimiter.length());
    }
    list.push_back(s);
    command_args.push_back(list);
  }
  original_file.close();
}

void condenseFlags(vector<vector<string>>& command_args) {
  const size_t num_commands = command_args.size();
  vector<string> flags;
  vector<vector<bool>> hasFlag;  // bool in nested vector corresponds to flag in flags

  // iterate over each command
  for (vector<string> command : command_args) {
    if (command[0].compare("gcc") == 0) {
      // iterate over each argument
      for (int i = 1; i < command.size(); i++) {
        string arg = command[i];
      }
    } else if (command[0].compare("/bin/sh")) {
      int i = 2;
      string arg = command[i];
      bool isCompileCommand = true;
      while (arg.compare("gcc") != 0) {
        i++;
        if (i > command.size()) {
          isCompileCommand = false;
          break;
        }
        arg = command[i];
      }
      if (!isCompileCommand) {
        break;
      }
      for (; i < command.size(); i++) {
        arg = command[i];
        int j;
        for (j = 0; j < flags.size(); j++) {
          if (flags[j].compare(arg) == 0) {
            hasFlag[i][j] = true;
            break;
          } else {
            hasFlag[i][j] = false;
          }
        }
        if (j == flags.size()) {
          flags.push_back(arg);
          hasFlag[i][j] = true;
        }
      }
    }
  }
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

fstream createRikerfile() {
  // Create empty Rikerfile
  fstream write_file(rikerfile_name.c_str(), fstream::trunc | fstream::out);

  // Write shell header to Rikerfile
  write_file << "#!/bin/sh" << endl << endl;
  // write_file.close();

  // Pipe make commands into Rikerfile
  // string command = "make -n --always-make >> " + rikerfile_name;
  // pclose(popen(command.c_str(), "w"));
  return write_file;
}

void processRikerfile() {
  // Filter out non-command messages
  // removeComments();

  fstream command_file("commands.txt", fstream::trunc | fstream::out);
  vector<vector<string>> command_args;
  getCommandArgs(command_args);
  for (vector<string> command : command_args) {
    for (string arg : command) {
      if (arg.compare("\\\\n") != 0) {
        command_file << arg << " ";
      }
    }
    command_file << endl;
  }

  // Still a work in progress for condensing flags
  // condenseFlags(command_args);

  formatShellCommands();
}

void finalizeRikerfile() {
  // Make the Rikerfile executable by calling "chmod u+x Rikerfile".
  vector<string> vs = {"chmod", "u+x", rikerfile_name};
  executeCommand("chmod", vs);
}

bool doesStrStartWithToken(string str, string token, const char* delimiter = " ") {
  string first_token = str.substr(0, str.find(delimiter));
  return first_token.compare(token) == 0;
}

void extractMakeCommands(shared_ptr<Command> root_cmd,
                         fstream& output,
                         bool child_of_make = false) {
  if (child_of_make && !root_cmd->isEmptyCommand()) {
    // Don't print the make command itself
    if (!doesStrStartWithToken(root_cmd->getShortName(), "make") &&
        root_cmd->getShortName().compare("./config.status")) {
      output << root_cmd->getFullName() << endl;
      // root_cmd->getOutputConsumers();
    }
  }

  for (auto child : root_cmd->getChildren()) {
    // If root_cmd arg 0 is "make" (or path to make), store children in list of commands for
    // rikerfile
    if (doesStrStartWithToken(root_cmd->getShortName(), "make")) {
      extractMakeCommands(child, output, true);
    } else {
      extractMakeCommands(child, output, false);
    }
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

  fstream rikerfile = createRikerfile();
  // fstream write_file(rikerfile_name.c_str(), fstream::trunc | fstream::out);
  // write_file << "#!/bin/sh" << endl << endl;

  extractMakeCommands(root_cmd, rikerfile, false);
  rikerfile.close();
  processRikerfile();

  finalizeRikerfile();
}