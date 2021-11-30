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

void condenseCompiles(vector<vector<string>>& command_args) {
  int num_commands = command_args.size();
  vector<string> flags;
  vector<vector<bool>> hasFlag;  // bool in nested vector corresponds to flag in flags

  // Extract the compile commands with -c flag and no linking
  vector<vector<string>> candidate_commands;
  // For each candidate_command, the first string is the full src file
  // name/location, the second is the object file directory
  vector<vector<string>> candidate_command_locs;
  for (int c_index = 0; c_index < num_commands; c_index++) {
    // iterate over each command
    vector<string> command(command_args[c_index]);

    if (isCompileCommand(command)) {
      size_t num_args = command.size();
      string o_loc, c_loc, o_root, c_root;
      for (int i = 0; i < num_args; i++) {
        if (command[i].compare("-o") == 0) {
          size_t slash = command[i + 1].find_last_of("/");
          if (slash != string::npos) {
            o_loc = command[i + 1].substr(0, slash + 1);
            o_root = command[i + 1].substr(slash + 1, command[i + 1].find_last_of(".") - slash - 1);
          } else {
            o_loc = ".";
            o_root = command[i + 1].substr(0, command[i + 1].find_last_of("."));
          }
          // command[i] = "";
          // command[i + 1] = "";
        } else if (hasEnding(command[i], ".cc") || hasEnding(command[i], ".c")) {
          c_loc = command[i];
          size_t slash = command[i].find_last_of("/");
          if (slash != string::npos) {
            c_root = command[i].substr(slash + 1, command[i].find_last_of(".") - slash - 1);
          } else {
            c_root = command[i + 1].substr(0, command[i + 1].find_last_of("."));
          }
        }
      }
      if (!c_root.empty() && (o_root.empty() || o_root.compare(c_root) == 0)) {
        candidate_commands.push_back(command);
        vector<string> locs = {c_loc, o_loc};
        candidate_command_locs.push_back(locs);
      }
    }
  }

  size_t num_candidates = candidate_commands.size();
  // Match candidate commands with commands with the same flags
  vector<int> matched(candidate_commands.size(), 0);
  int num_matched = 0;
  // fstream new_file("CondenseCompilesTest", fstream::trunc | fstream::out);
  // Iterate over each candidate command
  for (int c1_index = 0; c1_index < num_candidates; c1_index++) {
    // If current command hasn't been matched with any prior commands
    if (!matched[c1_index]) {
      // new_file << c1_index << endl;
      vector<string> command1 = candidate_commands[c1_index];
      string dir1 = candidate_command_locs[c1_index][1];
      // string dir1 = getCompileDirectory(command1)[0];
      // dir1 = dir1.substr(0, dir1.find_last_of("/"));
      // Check if it matches with any subsequent unmatched commands
      for (int c2_index = c1_index + 1; c2_index < num_candidates; c2_index++) {
        if (!matched[c2_index]) {
          // new_file << "  " << c2_index << endl;
          bool c2_match = true;
          vector<string> command2 = candidate_commands[c2_index];
          string dir2 = candidate_command_locs[c2_index][1];
          // string dir2 = getCompileDirectory(command2)[0];
          // dir2 = dir2.substr(0, dir2.find_last_of("/"));
          cout << dir1 << " " << dir2 << endl;
          if (dir1.compare(dir2) != 0) {
            c2_match = false;
          } else {
            for (int arg1_index = 1; arg1_index < command1.size(); arg1_index++) {
              string arg1 = command1[arg1_index];
              if (arg1.compare("") == 0) {
                continue;
              } else if (hasEnding(arg1, ".cc") || hasEnding(arg1, ".c")) {
                continue;
              } else if (isOneOf(arg1, {"-o"})) {
                arg1_index++;
                continue;
              } else if (!isOneOf(arg1, command2)) {
                // new_file << "    " << arg1 << " not part of command" << endl;
                cout << arg1 << endl;
                c2_match = false;
                break;
              }
            }
          }
          if (c2_match) {
            if (!matched[c1_index]) {
              num_matched++;
              matched[c1_index] = num_matched;
            }
            matched[c2_index] = num_matched;
          }
        }
      }
    }
  }

  if (num_matched > 0) {
    // Group matching src files for condensed compilation
    vector<vector<string>> matched_src_files(num_matched);
    vector<string> matched_obj_locs(num_matched);
    for (int i = 0; i < num_candidates; i++) {
      // Iterate over each candidate

      if (matched[i]) {
        vector<string> command = candidate_commands[i];
        vector<string> locs = candidate_command_locs[i];
        int src_index = matched[i] - 1;
        matched_src_files[src_index].push_back(locs[0]);
        matched_obj_locs[src_index] = locs[1];
        // // Find the src file
        // for (int j = 1; j < command.size(); j++) {
        //   string arg = command[j];
        //   if (isOneOf(arg, {"-o", "-c"})) {
        //     matched_src_files[src_index].push_back(command[j + 1]);
        //   }
        // }
      }
    }

    // Construct src vars of the form SRC#="a.cc b.cc c.cc ... z.cc"
    vector<string> src_vars(num_matched);
    vector<string> dir_vars(num_matched);
    vector<string> backtrack_vars(num_matched);
    for (int group_i = 0; group_i < num_matched; group_i++) {
      vector<string> grouped_files = matched_src_files[group_i];
      string var = "SRC" + std::to_string(group_i + 1) + "=\"";
      src_vars[group_i].append(var);

      // string dir_var = "# Files located in base Directory";
      // size_t end_i = grouped_files[0].find_last_of("/");
      // if (end_i != string::npos) {
      //   string dir = grouped_files[0].substr(0, end_i + 1);
      //   dir_var = "DIR" + std::to_string(group_i + 1) + "=\"" + dir + "\"";
      // }
      string obj_loc = matched_obj_locs[group_i];
      string dir_var = "DIR" + std::to_string(group_i + 1) + "=\"" + obj_loc + "\"";
      dir_vars[group_i] = dir_var;

      string backtrack = "";
      size_t pos = 1;
      string temp(obj_loc);
      while ((pos = temp.find("/", 1)) != string::npos) {
        backtrack += "../";
        temp.erase(0, pos);
      }
      backtrack_vars[group_i] = backtrack;
      for (int j = 0; j < grouped_files.size(); j++) {
        string to_append = backtrack + grouped_files[j];
        // size_t start = grouped_files[j].find_last_of("/");
        // if (start != string::npos) {
        //   to_append = grouped_files[j].substr(start + 1, grouped_files[j].size() - start);
        // } else {
        //   to_append = grouped_files[j];
        // }
        if (j == grouped_files.size() - 1) {
          to_append += "\"";
        } else {
          to_append += " ";
        }
        cout << to_append << endl;
        src_vars[group_i].append(to_append);
      }
    }

    // fstream new_file("CondenseCompilesTest", fstream::trunc | fstream::out);
    // for (int i = 0; i < num_candidates; i++) {
    //   new_file << matched[i] << " ";
    //   vector<string> command = candidate_commands[i];
    //   for (int j = 0; j < command.size(); j++) {
    //     new_file << command[j] << " ";
    //   }
    //   new_file << endl;
    // }

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

    // Remove condensed (redundant) compile commands
    while (getline(original_file, line)) {
      string line_copy(line);
      bool remove_compile = false;
      if (isCompileCommand(line_copy)) {
        for (int i = 0; i < matched_src_files.size(); i++) {
          vector<string> grouped_files = matched_src_files[i];
          for (int j = 0; j < grouped_files.size(); j++) {
            string c_file = grouped_files[j];
            if (line_copy.find(c_file) != string::npos) {
              remove_compile = true;
              if (j == grouped_files.size() - 1) {
                // If the compile command is the first of the group,
                // replace it with the condensed compile
                string to_insert(line);

                // Append backtrack to the directories of -I flags
                if (backtrack_vars[j].compare("./") != 0) {
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

                if (dir_vars[i].compare("# Files located in base Directory") != 0) {
                  size_t first = dir_vars[i].find_first_of("\"");
                  size_t last = dir_vars[i].find_last_of("/");
                  new_file << "cd "
                           //         << "$DIR" + std::to_string(i + 1) << endl;
                           << dir_vars[i].substr(first + 1, last - first) << endl;
                  new_file << to_insert << endl;
                  new_file << "cd -" << endl;
                } else {
                  new_file << to_insert << endl;
                }
              }
              break;
            }
          }
          if (remove_compile) {
            break;
          }
        }
      }
      if (!remove_compile) {
        new_file << line_copy << endl;
      }
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
void processRikerfile(bool skip_condencing) {
  // Get commands from Rikerfile in accessible format
  vector<vector<string>> command_args;
  getCommandArgs(command_args);

  // Check for common flags and replace them with $CFLAGS var
  // condenseFlags(command_args);
  if (!skip_condencing) {
    condenseCompiles(command_args);
  }

  // Format /bin/sh -c commands
  formatShellCommands();
}

// Make the Rikerfile executable by calling "chmod u+x Rikerfile".
void finalizeRikerfile() {
  vector<string> vs = {"chmod", "u+x", rikerfile_name};
  executeCommand("chmod", vs, false);
}

// This function runs when rkr generate is invoked
void do_generate(vector<string> args, bool skip_condencing) noexcept {
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

  cout << skip_condencing << endl;
  // Format Rikerfile for improved readability
  processRikerfile(skip_condencing);

  // Make Rikerfile Executable
  finalizeRikerfile();
}