#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include <string.h>
#include <unistd.h>

#include "util/options.hh"

using std::nullopt;
using std::optional;
using std::string;
using std::vector;

/// Keep a vector of entries in the PATH environment variable
vector<string> path;

/// Is this a wrapper around clang?
bool is_clang = false;

// STOLEN from wrapper.cc
void init_path() {
  std::string newpath = "";

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

int main(int argc, char* argv[]) {
  // Combine machine specific called remot
  char* remote_riker_path = getenv("RKR_REMOTE_PATH");

  init_path();

  char* command[argc + 10];  // save space for commands given + trace command

  // TODO: Handle PATH so that we can call the user's ssh instead of slogin
  command[0] = strdup("ssh");
  int cIndex = 1;  // index for custom arguments
  int aIndex = 1;  // index for command line arguments
  int dashCount = 0;

  for (; aIndex < argc; aIndex++) {
    // if we find argument that doesn't start with -
    if (argv[aIndex][0] != '-') {
      // add to the count
      dashCount++;
    }
    // The first two arguments without a dash (-) at the front should represent the address for the
    // ssh and the requested command. The tracing program must thus be inserted between these, if it
    // has been called by the -r flag.
    if (dashCount == 2 && remote_riker_path != NULL) {
      // bring in the path to remote-trace within riker and combine with path to riker on remote
      const char* remote_detail_path = "/debug/share/rkr/\\remote-trace";
      char full_path[strlen(remote_riker_path) + strlen(remote_detail_path)];
      strcpy(full_path, remote_riker_path);
      strcat(full_path, remote_detail_path);

      // add argument to run remote-trace
      command[cIndex] = strdup(full_path);
      cIndex++;
      dashCount++;

      // parse argument normally
      command[cIndex] = strdup(argv[aIndex]);
      cIndex++;
    }
    // parse argument normally
    else {
      command[cIndex] = strdup(argv[aIndex]);
      cIndex++;
    }
  }

  // end the array with null
  command[cIndex] = (char*)NULL;

  // execute the command
  execvp("ssh", command);

  for (int i = 0; i < cIndex; i++) {
    free(command[i]);
  }
  return 0;
}
