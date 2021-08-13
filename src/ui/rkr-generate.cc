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

// Name of resulting Rikerfile
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

// This function splits Rikerfile into a vector of commands (each line), where the commands are
// vectors of arguments.
void getCommandArgs(vector<vector<string>>& command_args) {
  fstream original_file(rikerfile_name.c_str(), fstream::in);
  string line, token, delimiter = " ";
  getline(original_file, line);

  // Iterate over every line in the Rikerfile
  while (getline(original_file, line)) {
    // Remove any new line characters
    replaceToken(line, "\\\\n", "");
    vector<string> list;
    string s = line;
    size_t pos = 0;
    string token;
    while ((pos = s.find(delimiter)) != string::npos) {
      // Break up line into tokens at spaces
      token = s.substr(0, pos);
      // Remove unnecessary spaces
      replaceToken(token, " ", "");
      list.push_back(token);
      s.erase(0, pos + delimiter.length());
    }
    list.push_back(s);
    command_args.push_back(list);
  }
  original_file.close();
}

// Finds flags shared between every compile command
// Replaces all occurrences with $CFLAGS var
// Note: While shared flags are determined by regular "gcc/clang/etc" calls, any occurrences will be
// replaced with var (like /bin/sh/ -c commands).
void condenseFlags(vector<vector<string>>& command_args) {
  int num_commands = command_args.size();
  vector<string> flags;
  vector<vector<bool>> hasFlag;  // bool in nested vector corresponds to flag in flags
  int num_compile_commands = 0;
  for (int c_index = 0; c_index < num_commands; c_index++) {
    // iterate over each command
    vector<string> command(command_args[c_index]);

    if ((command[0].compare("gcc") == 0) || (command[0].compare("g++") == 0) ||
        (command[0].compare("clang") == 0) || (command[0].compare("clang++") == 0) ||
        (command[0].compare("cc") == 0) || (command[0].compare("c++") == 0)) {
      // Current Command can potentially be condensed

      num_compile_commands++;
      vector<bool> command_flags(flags.size());
      hasFlag.push_back(command_flags);

      for (int i = 1; i < command.size(); i++) {
        // Iterate over each argument of the command
        string arg = command[i];

        if (arg.compare("-o") == 0) {
          i++;
          continue;
        } else if (!(hasEnding(arg, ".c") || hasEnding(arg, ".cc") || hasEnding(arg, ".o"))) {
          // A potential flag has been found
          bool new_flag = true;
          if (!flags.empty()) {
            int flag_index;
            for (flag_index = 0; flag_index < flags.size(); flag_index++) {
              // Iterate over each flag found so far
              string flag = flags[flag_index];
              if (flag.compare(arg) == 0) {
                new_flag = false;
                hasFlag[num_compile_commands - 1][flag_index] = true;
                break;
              }
            }
          }
          if (new_flag) {
            // If flag is new, all prior commands lacked it
            flags.push_back(arg);
            for (int z = 0; z < num_compile_commands - 1; z++) {
              hasFlag[z].push_back(false);
            }
            hasFlag[num_compile_commands - 1].push_back(true);
          }
        }
      }
    }
    /*
    // Handle compile commands run using "/bin/sh -c"
    } else if (command[0].compare("/bin/sh")) {
      int i = 2;
      string arg = command[i];
      bool isCompileCommand = true;
      while (arg.compare("gcc") != 0) {
        i++;
        if (i >= command.size()) {
          isCompileCommand = false;
          break;
        }
        arg = command[i];
      }
      if (isCompileCommand) {
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
    }*/
  }

  // Find flags that every compile command uses
  vector<string> cflags;
  for (int i = 0; i < flags.size(); i++) {
    // Iterate over each flag found

    bool pervasive_flag = true;
    for (int j = 0; j < num_compile_commands; j++) {
      // Iterate over each command to check for flag
      if (!hasFlag[j][i]) {
        pervasive_flag = false;
        break;
      }
    }
    // If all commands have the flag, add it to cflags var
    if (pervasive_flag) {
      cflags.push_back(flags[i]);
    }
  }

  if (cflags.size() >= 2) {
    // Replace common flags with CFLAGS var
    string cflags_str;
    for (int i = 0; i < cflags.size() - 1; i++) {
      cflags_str += cflags[i] + " ";
    }
    cflags_str += cflags[cflags.size() - 1];

    string temp_name = "temp.txt";
    fstream original_file(rikerfile_name.c_str(), fstream::in);
    fstream new_file(temp_name, fstream::trunc | fstream::out);

    string line, token, delimiter = " ";

    // Modify header to include CFLAGS var
    getline(original_file, line);
    new_file << "#!/bin/sh" << endl << endl;
    new_file << "CFLAGS=\"" << cflags_str << "\"" << endl;

    // Check each line for cflags_str, and replace with $CFLAGS
    // Note: Assumes shared flags occur in a continuous string in the order they first appeared.
    // If flags are separated/in a different order, it won't be replaced
    while (getline(original_file, line)) {
      string line_copy(line);

      replaceToken(line_copy, cflags_str, "$CFLAGS");

      new_file << line_copy << endl;
    }
    remove(rikerfile_name.c_str());
    rename(temp_name.c_str(), rikerfile_name.c_str());
  }
}

// Helper function to execute a shell command in a new thread
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

// Helper function to check a command (by its first arg)
bool doesStrStartWithToken(string str, string token, const char* delimiter = " ") {
  string first_token = str.substr(0, str.find(delimiter));
  return first_token.compare(token) == 0;
}

// Recursively scans trace and extracts first layer of Make commands
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

  return write_file;
}

// Processes and modifies Rikerfile for improved readability
void processRikerfile() {
  // Get commands from Rikerfile in accessible format
  vector<vector<string>> command_args;
  getCommandArgs(command_args);

  // Check for common flags and replace them with $CFLAGS var
  condenseFlags(command_args);

  // Format /bin/sh -c commands
  formatShellCommands();
}

// Make the Rikerfile executable by calling "chmod u+x Rikerfile".
void finalizeRikerfile() {
  vector<string> vs = {"chmod", "u+x", rikerfile_name};
  executeCommand("chmod", vs);
}

// This function runs when rkr generate is invoked
void do_generate(vector<string> args) noexcept {
  // Turn on input/output tracking
  options::track_inputs_outputs = true;

  // Reset the stats counters
  reset_stats();

  // Load the serialized build trace
  auto [root_cmd, trace] = InputTrace::load(constants::DatabaseFilename, args);

  // Emulate the trace
  trace->sendTo(Build());

  // Create new Rikerfile
  fstream rikerfile = createRikerfile();

  // Extract build commands from rkr trace
  extractMakeCommands(root_cmd, rikerfile, false);
  rikerfile.close();

  // Format Rikerfile for improved readability
  processRikerfile();

  // Make Rikerfile Executable
  finalizeRikerfile();
}