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

int main(int argc, char* argv[]) {
  // Combine machine specific called remot
  // If remote path is not set, null will be returned
  // and remote tracing deactivated
  char* remote_riker_path = getenv("RKR_REMOTE_PATH");
  char* remote_riker_args = getenv("RKR_REMOTE_ARGS");

  std::cout << "\nget to here\n";

  init_path();

  char* command[argc + 10];  // save space for commands given + trace command

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
      const char* remote_detail_path = "/../share/rkr/\\remote-trace";
      char full_path[strlen(remote_riker_path) + strlen(remote_detail_path)];
      strcpy(full_path, remote_riker_path);
      strcat(full_path, remote_detail_path);

      // add argument to run remote-trace
      command[cIndex] = strdup(full_path);
      cIndex++;
      dashCount++;

      // pass the flags if user spicify in local machines
      if (remote_riker_args != NULL) {
        char option_arg[strlen(remote_riker_args)];
        strcpy(option_arg, remote_riker_args);

        // add argument to run remote-trace
        command[cIndex] = strdup(option_arg);
        cIndex++;
      }

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

  std::cout << "command: ";
  for (int i = 0; i < cIndex; i++) {
    std::cout << command[i] << " ";
  }
  std::cout << "\n";

  // execute the command
  execvp("ssh", command);

  for (int i = 0; i < cIndex; i++) {
    free(command[i]);
  }
  return 0;
}
