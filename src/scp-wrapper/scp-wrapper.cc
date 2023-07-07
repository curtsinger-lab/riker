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

int main(int argc, char** argv) {
  init_path();
  
  // Can also use /usr/bin/ssh if slogin is not available
  std::string commandbuild = "scp";

  // Use option -S to use riker's ssh instead of scp's auto use of system ssh
  commandbuild = commandbuild + " -S ssh";

  // TODO: Allow multiple command line arguments for scp
  for (int i = 1; i < argc; ++i) 
  	commandbuild = commandbuild + " " + argv[i];

  const char* command = commandbuild.c_str();
  // TODO: Switch this to an exec call with PATH handling
  system(command);

  return 0;
}
