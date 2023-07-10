#include <filesystem>
#include <optional>
#include <string>
#include <vector>

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

namespace fs = std::filesystem;

using std::nullopt;
using std::optional;
using std::string;
using std::vector;

/// Keep a vector of entries in the PATH environment variable
vector<string> path;

/// Is this a wrapper around clang?
bool is_clang = false;

void init_path() {
  string newpath = "";

  if (char* path_str = getenv("PATH"); path_str != NULL) {
    string old_path(path_str);

    // Split the path at colon characters until no separators remain
    size_t start = 0;
    while (start < old_path.size()) {
      // Look for a colon separator
      size_t sep = old_path.find(':', start);

      // If we didn't find a colon, put the separtor at the end of the string
      if (sep == string::npos) sep = old_path.size();

      // Grab the next part of the path variable
      const auto part = old_path.substr(start, sep - start);

      // If the part is not a path to the wrappers directory, include it
      if (part.find("share/rkr/wrappers") == string::npos) {
        // Add the part to the path vector
        path.push_back(part);
        if (newpath.size() > 0) newpath += ':';
        newpath += part;
      }

      // Move past the colon separator
      start = sep + 1;
    }
  }

  // Update the PATH environment variable
  setenv("PATH", newpath.c_str(), 1);
}

typedef int (*execve_fn_t)(const char*, char* const*, char* const*);

/// Make an untraced execve system call.
int execv_untraced(const char* pathname, char* const* argv) {
  // Try to get the untrace execve function from the injected library
  static execve_fn_t _fn = reinterpret_cast<execve_fn_t>(dlsym(RTLD_NEXT, "execve_untraced"));

  // Did we find the injected library function?
  if (!is_clang && _fn) {
    // Yes. Use it.
    return _fn(pathname, argv, environ);

  } else {
    // No. Fall back on a regular execve. This will break some unit tests, but works fine.
    return execve(pathname, argv, environ);
  }
}

/// Make an untraced execvpe system call. Loop through PATH entries to issue each exec.
int execvp_untraced(const char* pathname, char* const* argv) {
  // Does pathname contain a slash character?
  if (strchr(pathname, '/') != NULL) {
    // Yes. Do not search PATH
    execv_untraced(pathname, argv);

  } else {
    // Try PATH entries in order
    for (const auto& entry : path) {
      auto new_pathname = entry + '/' + pathname;
      execv_untraced(new_pathname.c_str(), argv);
    }
  }

  return -1;
}

/// Make an untraced execvpe system call from a vector of strings
int execvp_untraced(const vector<string>& args) {
  char* argv[args.size() + 1];
  for (size_t i = 0; i < args.size(); i++) {
    argv[i] = const_cast<char*>(args[i].data());
  }
  argv[args.size()] = NULL;
  return execvp_untraced(argv[0], argv);
}

bool one_of(const string& str, const std::initializer_list<string>& options) {
  for (const auto& option : options) {
    if (str == option) return true;
  }
  return false;
}

bool has_prefix(const string& str, const std::initializer_list<string>& prefixes) {
  for (const auto& prefix : prefixes) {
    // If the argument is shorter than the prefix, do not check
    if (str.size() < prefix.size()) continue;

    // Does the argument start with the prefix?
    if (str.substr(0, prefix.size()) == prefix) {
      return true;
    }
  }
  return false;
}

bool has_suffix(const string& str, const std::initializer_list<string>& suffixes) {
  for (const auto& suffix : suffixes) {
    // If the argument is shorter than the suffix, do not check
    if (str.size() < suffix.size()) continue;

    // Does the argument end with the suffix?
    if (str.substr(str.size() - suffix.size(), suffix.size()) == suffix) {
      return true;
    }
  }
  return false;
}

optional<int> assemble(vector<string>& args, vector<string>& tempfiles) {
  return nullopt;
}

optional<int> compile(vector<string>& args, vector<string>& tempfiles) {
  vector<string> compile_args;
  vector<string> source_files;
  vector<string> output_files;

  // Was the -c flag passed in?
  bool compile_flag = false;

  // The path to the output file, if one was provided
  optional<string> output_option;

  // Start processing arguments
  auto arg_iter = args.begin();

  // Save the compiler name first
  compile_args.push_back(*arg_iter);
  arg_iter++;

  // Loop over remaining arguments
  while (arg_iter != args.end()) {
    auto& arg = *arg_iter;

    // Inspect the argument
    if (arg == "-o") {
      // Skip over -o and the following entry
      arg_iter++;

      // Save the output file option for later
      output_option = *arg_iter;

    } else if (arg == "-c") {
      // This is a compilation command, not a full linking command
      compile_flag = true;

    } else if (has_prefix(arg, {"-l", "-L"}) || one_of(arg, {"-shared", "-fstandalone-debug"})) {
      // Skip linker arguments

    } else if (has_suffix(arg, {".o", ".a", ".so"})) {
      // Skip linkable files

    } else if (has_suffix(arg, {".c", ".cc", ".cpp", ".c++", ".s", ".S"})) {
      // Capture source files
      source_files.push_back(arg);

      // Create a temporary .o file path
      string tempname = fs::path(arg).filename().stem().string() + "-XXXXXX.o";
      string output = (fs::temp_directory_path() / tempname).string();
      int rc = mkstemps(output.data(), 2);
      if (rc == -1) {
        perror("Failed to create temporary file");
        return EXIT_FAILURE;
      }
      close(rc);

      output_files.push_back(output);

      // Change the args so the linker stage uses the .o file instead of the source file
      *arg_iter = output;

    } else {
      compile_args.push_back(arg);
    }

    // Move to the next argument
    arg_iter++;
  }

  // Handle compile commands
  if (compile_flag) {
    // Was an output filename provided?
    if (output_option.has_value()) {
      // Make sure there's only one source file
      if (source_files.size() != 1) {
        fprintf(stderr, "Invalid combination of -c and -o flags with multiple sources.\n");
        return 1;
      }

      // Swap in the new output file name
      output_files[0] = output_option.value();

    } else {
      // Replace the temporary output files with names based on the source file
      for (size_t i = 0; i < source_files.size(); i++) {
        output_files[i] = fs::path(source_files[i]).filename().stem().string() + ".o";
      }
    }
  }

  size_t launched = 0;
  size_t finished = 0;
  size_t jobs = 12;
  optional<int> exit_code = nullopt;

  while (finished < source_files.size()) {
    // Can we launch a job now?
    if (launched < source_files.size() && launched - finished < jobs) {
      vector<string> new_args = compile_args;
      new_args.push_back("-o");
      new_args.push_back(output_files[launched]);
      new_args.push_back("-c");
      new_args.push_back(source_files[launched]);

      pid_t child_id = fork();
      if (child_id == -1) {
        perror("fork failed");
        return EXIT_FAILURE;

      } else if (child_id == 0) {
        // In the child
        execvp_untraced(new_args);

        // Print an error message if the compiler did not exec
        fprintf(stderr, "rkr-wrapper failed to launch %s: ", new_args[0].c_str());
        perror("");

        exit(EXIT_FAILURE);
      }

      // One more job is launched
      launched++;

    } else {
      // We've hit the maximum number of simultaneous jobs. Wait for one
      int status;
      if (wait(&status) == -1) {
        perror("wait failed");
        return EXIT_FAILURE;
      }

      // If the child failed, the compiler wrapper should fail too
      if (WEXITSTATUS(status) != 0) {
        exit_code = WEXITSTATUS(status);
      }

      // One more job is finished
      finished++;
    }
  }

  // If the -c flag was passed in, exit with success unless there was some prior error
  if (compile_flag && !exit_code.has_value()) {
    exit_code = 0;
  }

  return exit_code;
}

optional<int> link(vector<string> args, vector<string>& tempfiles) {
  // Create a child process to run the linking step
  pid_t child_id = fork();

  // Check the return value from fork()
  if (child_id == -1) {
    perror("fork failed");
    return EXIT_FAILURE;

  } else if (child_id == 0) {
    // In the child. Use execvp to run the linker
    execvp_untraced(args);

    // Print an error message if the linker did not exec
    fprintf(stderr, "rkr-wrapper failed to launch %s: ", args[0].c_str());
    perror("");

    exit(EXIT_FAILURE);

  } else {
    // In the parent. Wait for the linking stage to finish
    int status;
    pid_t rc = wait(&status);
    if (rc == -1) {
      perror("wait failed");
      return EXIT_FAILURE;
    } else {
      return WEXITSTATUS(status);
    }
  }
}

/// Break the wrapped compilation into separate, parallel steps
int main(int argc, char* argv[]) {
  // Process the PATH environment variable. Remove this wrapper from the PATH.
  init_path();

  // Make a vector of args
  vector<string> args(argv, argv + argc);

  // Is this a wrapper around clang?
  is_clang = (args[0] == "clang" || args[0] == "clang++");

  // TODO: if cc or c++ is a link to clang we'd want to detect that

  // Set up a container for temporary file paths we need to clean up
  vector<string> tempfiles;

  // Run assembly commands
  optional<int> exit_code = assemble(args, tempfiles);

  // Run compilation steps unless assembly exited
  if (!exit_code.has_value()) {
    exit_code = compile(args, tempfiles);
  }

  // Run the linking step unless compilation exited
  if (!exit_code.has_value()) {
    exit_code = link(args, tempfiles);
  }

  // TODO: clean up temporary files

  // Exit with the provided exit code (or 0 by default)
  return exit_code.value_or(0);
}
