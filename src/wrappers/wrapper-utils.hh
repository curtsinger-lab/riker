#pragma once
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

// Moves working path out of riker. Originally created in compiler-wrapper.
static void init_path() {
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