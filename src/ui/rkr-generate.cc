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
    replaceToken(line, "  ", " ");
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

// Returns if to_compare contains s1.
bool isOneOf(string s1, vector<string> to_compare) {
  for (string s2 : to_compare) {
    if (s1.compare(s2) == 0) {
      return true;
    }
  }
  return false;
}

// Returns a boolean if the first token of a command is a compiler call
bool isCompileCommand(vector<string> command) {
  vector<string> compilers{"gcc", "g++", "clang", "clang++", "cc", "c++"};
  return isOneOf(command[0], compilers);
}

// Returns a boolean if the first token of a command is a compiler call
bool isCompileCommand(string command) {
  vector<string> compilers{"gcc", "g++", "clang", "clang++", "cc", "c++"};
  string command_token = command.substr(0, command.find(" "));
  return isOneOf(command_token, compilers);
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

    if (isCompileCommand(command)) {
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

    // Create string of CFLAGS for CFLAGS var
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
      bool replace_line = true;
      for (string flag : cflags) {
        if (line_copy.find(flag) != string::npos) {
          replaceToken(line_copy, flag, "");
        } else {
          replace_line = false;
          break;
        }
      }
      if (replace_line) {
        size_t pos = 0;
        if (line_copy.find("/bin/sh -c") != string::npos) {
          string line_start = "/bin/sh -c ";
          pos = line_start.size();
        }
        line_copy.replace(line_copy.find(" ", pos), 1, " $CFLAGS ");
        new_file << line_copy << endl;

      } else {
        new_file << line << endl;
      }
    }
    remove(rikerfile_name.c_str());
    rename(temp_name.c_str(), rikerfile_name.c_str());
  }
}

bool argInCommand(string arg, vector<string> command) {
  for (string arg2 : command) {
    if (arg.compare(arg2) == 0) {
      return true;
    }
  }
  return false;
}

// getCompileDirectory() takes in a compile command and returns a vector<string> containing the
// compile target [0] and origin [1].
vector<string> getCompileDirectory(vector<string> command) {
  vector<string> to_return = {"base target", "base origin"};

  for (int i = 0; i < command.size(); i++) {
    string arg(command[i]);
    if (hasEnding(arg, ".o")) {
      size_t end = arg.find_last_of('/');
      if (end != string::npos) {
        to_return[0] = arg.substr(0, end + 1);
      }
    } else if (hasEnding(arg, ".cc") || hasEnding(arg, ".cpp")) {
      size_t end = arg.find_last_of('/');
      if (end != string::npos) {
        to_return[1] = arg.substr(0, end + 1);
      }
      // cout << "dir: " << to_return << endl;
    }
  }
  return to_return;
}

// flagMatch() checks if
bool flagsMatch(vector<string> command1, vector<string> command2) {
  for (int arg1_index = 1; arg1_index < command1.size(); arg1_index++) {
    string arg1 = command1[arg1_index];
    if (arg1.compare("") == 0) {
      continue;
    } else if (hasEnding(arg1, ".cc") || hasEnding(arg1, ".c")) {
      continue;
    } else if (isOneOf(arg1, {"-o"})) {
      arg1_index++;
    } else if (!isOneOf(arg1, command2)) {
      return false;
    }
  }
  for (int arg2_index = 1; arg2_index < command2.size(); arg2_index++) {
    string arg2 = command2[arg2_index];
    if (arg2.compare("") == 0) {
      continue;
    } else if (hasEnding(arg2, ".cc") || hasEnding(arg2, ".c")) {
      continue;
    } else if (isOneOf(arg2, {"-o"})) {
      arg2_index++;
      continue;
    } else if (!isOneOf(arg2, command1)) {
      // new_file << "    " << arg1 << " not part of command" << endl;
      // cout << arg1 << endl;
      return false;
    }
  }
  return true;
}

// condenseCompiles groups compile commands with matching flags and compile target directories into
// single commands. The function takes in a vector of commands, where each line is a vector of
// strings.
void condenseCompiles(vector<vector<string>>& command_args) {
  // Step 1: Extract compile commands that can be condensed (no linking)
  // Initialize Variables
  int num_commands = command_args.size();
  vector<string> flags;
  vector<vector<bool>> hasFlag;
  // candidate_commands contains compile commands with -c flag and no linking
  vector<vector<string>> candidate_commands;
  // For each candidate_command, the first string is the full src file
  // name/location, the second is the object file directory
  vector<vector<string>> candidate_command_locs;

  // Iterate over each command in the generated Rikerfile
  for (int c_index = 0; c_index < num_commands; c_index++) {
    vector<string> command(command_args[c_index]);

    if (!isCompileCommand(command)) {
      continue;
    }

    // Iterate over each argument of compile command, extracting necessary info
    size_t num_args = command.size();
    string o_loc, c_loc = ".", o_name, c_name;
    for (int i = 0; i < num_args; i++) {
      // If -o found, next element defines compile target
      if (command[i].compare("-o") == 0) {
        size_t slash = command[i + 1].find_last_of("/");
        if (slash != string::npos) {
          // Extract both the target directory and .o file name
          o_loc = command[i + 1].substr(0, slash + 1);
          o_name = command[i + 1].substr(slash + 1, command[i + 1].find_last_of(".") - slash - 1);
        } else {
          o_name = command[i + 1].substr(0, command[i + 1].find_last_of("."));
        }
      }

      // If .c or .cc file found, extract source directory and name
      else if (hasEnding(command[i], ".cc") || hasEnding(command[i], ".c")) {
        c_loc = command[i];
        size_t slash = command[i].find_last_of("/");
        if (slash != string::npos) {
          c_name = command[i].substr(slash + 1, command[i].find_last_of(".") - slash - 1);
        } else {
          c_name = command[i + 1].substr(0, command[i + 1].find_last_of("."));
        }
      }
    }

    // If the target and source name are the same and necessary info was extracted, command is a
    // candidate for condensing.
    if (!c_name.empty() && (o_name.empty() || o_name.compare(c_name) == 0)) {
      candidate_commands.push_back(command);
      vector<string> locs = {c_loc, o_loc};
      candidate_command_locs.push_back(locs);
    }
  }

  // Step 2: Match candidate commands with identical flags and target directory

  size_t num_candidates = candidate_commands.size();
  vector<int> matched(candidate_commands.size(), 0);
  int num_matched = 0;

  // Iterate over each candidate command
  for (int c1_index = 0; c1_index < num_candidates; c1_index++) {
    // If current command has already been matched, skip
    if (matched[c1_index]) {
      continue;
    }

    vector<string> command1 = candidate_commands[c1_index];
    string dir1 = candidate_command_locs[c1_index][1];
    // Check if it matches with any subsequent unmatched commands
    for (int c2_index = c1_index + 1; c2_index < num_candidates; c2_index++) {
      if (matched[c2_index]) {
        continue;
      }

      vector<string> command2 = candidate_commands[c2_index];
      string dir2 = candidate_command_locs[c2_index][1];

      // Target directories must match to group
      if (dir1.compare(dir2) != 0) {
        continue;
      }

      // Flags must be identical to group
      if (flagsMatch(command1, command2)) {
        if (!matched[c1_index]) {
          num_matched++;
          matched[c1_index] = num_matched;
        }
        matched[c2_index] = num_matched;
      }
    }
  }

  // If no compile commands to condense, return.
  if (num_matched == 0) {
    return;
  }

  // Step 3: Construct condensed compile commands

  // Group matching src files for condensed compilation
  vector<vector<string>> matched_src_files(num_matched);
  vector<string> matched_obj_locs(num_matched);

  // Iterate over each candidate, transferring info into grouped locations
  for (int i = 0; i < num_candidates; i++) {
    if (matched[i]) {
      vector<string> command = candidate_commands[i];
      vector<string> locs = candidate_command_locs[i];
      int src_index = matched[i] - 1;
      matched_src_files[src_index].push_back(locs[0]);
      matched_obj_locs[src_index] = locs[1];
    }
  }

  // Construct src vars of the form SRC#="a.cc b.cc c.cc ... z.cc"
  vector<string> src_vars(num_matched);
  vector<string> dir_vars(num_matched);
  vector<string> backtrack_vars(num_matched);
  for (int group_i = 0; group_i < num_matched; group_i++) {
    vector<string> grouped_files = matched_src_files[group_i];
    string obj_loc = matched_obj_locs[group_i];
    string var = "SRC" + std::to_string(group_i + 1) + "=\"";
    src_vars[group_i].append(var);

    string backtrack = "";
    size_t pos = 1;
    string temp(obj_loc);
    while ((pos = temp.find("/", 1)) != string::npos) {
      backtrack += "../";
      temp.erase(0, pos);
    }
    backtrack_vars[group_i] = backtrack;

    if (grouped_files.size() == 0) {
      src_vars[group_i].append("\"");
    }
    // Attach SRC files to SRC var
    for (int j = 0; j < grouped_files.size(); j++) {
      string to_append = backtrack + grouped_files[j];
      if (j == grouped_files.size() - 1) {
        to_append += "\"";
      } else {
        to_append += " ";
      }
      src_vars[group_i].append(to_append);
    }

    if (obj_loc.compare("") == 0) {
      dir_vars[group_i] = "";
      backtrack_vars[group_i] = "";
      continue;
    }
    string dir_var = "DIR" + std::to_string(group_i + 1) + "=\"" + obj_loc + "\"";
    dir_vars[group_i] = dir_var;
  }

  string temp_name = "temp.txt";
  fstream original_file(rikerfile_name.c_str(), fstream::in);
  fstream new_file(temp_name, fstream::trunc | fstream::out);
  string line;

  // Copy #!/bin/sh and new line
  getline(original_file, line);
  new_file << line << endl;
  getline(original_file, line);
  new_file << line << endl;

  // Inject SRC vars
  for (int i = 0; i < num_matched; i++) {
    new_file << dir_vars[i] << endl;
    new_file << src_vars[i] << endl;
  }
  new_file << endl;

  // Remove condensed (redundant) compile commands and inject changes into generated Rikerfile
  while (getline(original_file, line)) {
    string line_copy(line);
    bool remove_compile = false;

    // If the command is not compiling, ignore it
    if (!isCompileCommand(line_copy)) {
      new_file << line_copy << endl;
      continue;
    }

    // If the command compiles, check if it has been condensed

    // Iterate over each grouping in matched_src_files
    for (int i = 0; i < matched_src_files.size(); i++) {
      vector<string> grouped_files = matched_src_files[i];

      // Iterate over each compile in the group
      for (int j = 0; j < grouped_files.size(); j++) {
        string c_file = grouped_files[j];

        // If the current command contains the c_file, we remove this compile (as it has been
        // condensed elsewhere)
        if (line_copy.find(c_file) != string::npos) {
          remove_compile = true;

          // If the compile command is the last of the group,
          // replace it with the condensed compile
          if (j != grouped_files.size() - 1) {
            break;
          }

          string to_insert(line);

          // Append backtrack to the directories of -I flags
          if (backtrack_vars[i].compare("") != 0) {
            size_t ind = 0;
            while ((ind = to_insert.find(" -I", ind + 1)) != string::npos) {
              to_insert.insert(ind + 3, backtrack_vars[j]);
            }
          }

          size_t start = to_insert.find(" -o");
          if (start != string::npos) {
            size_t end_o = to_insert.find(" ", start + 4);
            to_insert.replace(start, end_o - start, "");
          }
          start = to_insert.find(" -c");
          if (start != string::npos) {
            to_insert.insert(start, " -I" + backtrack_vars[j]);
          }
          string src_var = "$SRC" + std::to_string(i + 1);
          to_insert.replace(to_insert.find(c_file), c_file.size(), src_var);

          if (dir_vars[i].compare("") != 0) {
            size_t first = dir_vars[i].find_first_of("\"");
            size_t last = dir_vars[i].find_last_of("/");
            new_file << "cd " << dir_vars[i].substr(first + 1, last - first) << endl;
            new_file << to_insert << endl;
            new_file << "cd -" << endl;
          } else {
            new_file << to_insert << endl;
          }
        }

        // If we determined this line should be removed, don't send it to new Rikerfile
        if (remove_compile) {
          break;
        }
      }
      if (remove_compile) {
        break;
      }
    }

    // The compile is uncondensed, so keep it
    if (!remove_compile) {
      new_file << line_copy << endl;
    }
  }
  remove(rikerfile_name.c_str());
  rename(temp_name.c_str(), rikerfile_name.c_str());
}

// Helper function to execute a shell command in the same or new thread
void executeCommand(string path, vector<string> args, bool has_own_thread = false) {
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
void processRikerfile(bool skip_condensing) {
  // Get commands from Rikerfile in accessible format
  vector<vector<string>> command_args;
  getCommandArgs(command_args);

  // Check for common flags and replace them with $CFLAGS var
  // condenseFlags(command_args);
  if (!skip_condensing) {
    condenseCompiles(command_args);
  }

  // Format /bin/sh -c commands
  formatShellCommands();
}

// Make the Rikerfile executable by calling "chmod u+x Rikerfile".
void finalizeRikerfile() {
  vector<string> vs = {"chmod", "u+x", rikerfile_name};

  // Currently, chmod does not work as intended. Permission is denied
  executeCommand("chmod", vs, false);

  // char* pathC = getenv("PATH");
  // if (pathC != NULL) {
  //   path_update(pathC);
  // }
}

// This function runs when rkr generate is invoked. If option "--skip-condensing" is included,
// skip_condensing is true;
void do_generate(vector<string> args, bool skip_condensing) noexcept {
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

  // Extract build commands from rkr trace and put in Rikerfile
  extractMakeCommands(root_cmd, rikerfile, false);
  rikerfile.close();

  // Format Rikerfile for improved readability
  processRikerfile(skip_condensing);

  // Make Rikerfile Executable
  finalizeRikerfile();
}