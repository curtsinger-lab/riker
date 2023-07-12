#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include <string.h>
#include <unistd.h>

// TODO: make path non-relative
#include "../wrappers/wrapper-utils.hh"
#include "util/options.hh"

using std::nullopt;
using std::optional;
using std::string;
using std::vector;

int main(int argc, char** argv) {
  string full_path = getenv("PATH");
  size_t sep = full_path.find(':', 0);
  string riker_path = full_path.substr(0, sep);
  printf("path: %s\n", riker_path.c_str());

  init_path();

  // Can also use /usr/bin/ssh if slogin is not available
  std::string commandbuild = "scp";

  // Use option -S to use riker's ssh instead of scp's auto use of system ssh
  if (getenv("RKR_REMOTE_PATH") != NULL) {
    commandbuild = commandbuild + " -S " + riker_path + "/ssh";
  }

  // TODO: Allow multiple command line arguments for scp
  for (int i = 1; i < argc; ++i) commandbuild = commandbuild + " " + argv[i];

  const char* command = commandbuild.c_str();
  // TODO: Switch this to an exec call with PATH handling
  system(command);

  return 0;
}
