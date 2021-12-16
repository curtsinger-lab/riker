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
  if (_fn) {
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

    } else if (has_suffix(arg, {".c", ".cc", ".cpp", ".c++", ".s"})) {
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

    // TODO: omit .o, .a, .so, and linker arguments

    // Move to the next argument
    arg_iter++;
  }

  // Now launch compilation commands
  for (size_t i = 0; i < source_files.size(); i++) {
    vector<string> new_args = compile_args;
    new_args.push_back("-o");
    new_args.push_back(output_files[i]);
    new_args.push_back("-c");
    new_args.push_back(source_files[i]);

    pid_t child_id = fork();
    if (child_id == -1) {
      perror("fork failed");
      return EXIT_FAILURE;

    } else if (child_id == 0) {
      // In the child
      execvp_untraced(new_args);
      exit(EXIT_FAILURE);
    }
  }

  optional<int> exit_code = nullopt;

  // Wait for all compilation commands
  for (size_t i = 0; i < source_files.size(); i++) {
    int status;
    if (wait(&status) == -1) {
      perror("wait failed");
      return EXIT_FAILURE;
    }

    // If the child failed, the compiler wrapper should fail too
    if (WEXITSTATUS(status) != 0) {
      exit_code = WEXITSTATUS(status);
    }
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
    int rc = execvp_untraced(args);
    exit(rc);

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
