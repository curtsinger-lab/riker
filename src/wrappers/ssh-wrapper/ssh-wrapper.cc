#include <filesystem>
#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include <string.h>
#include <unistd.h>

// TODO: make path non-relative
#include "../wrappers/wrapper-utils.hh"
#include "util/options.hh"
namespace fs = std::filesystem;

using std::nullopt;
using std::optional;
using std::string;
using std::vector;

int main(int argc, char* argv[]) {
  // If remote path is not set, null will be returned
  // and remote tracing deactivated
  char* remote_riker_path = getenv("RKR_REMOTE_PATH");
  char* remote_riker_args = getenv("RKR_REMOTE_ARGS");

  // Remove wrappers from current path
  init_path();

  /* Iterate through passed arguments to find requested setup for ssh-session*/
  char* command[argc + 10];
  command[0] = strdup("ssh");
  int cIndex = 1;     // index for custom arguments
  int aIndex = 1;     // index for command line arguments
  int dashCount = 0;  // index for the # of arguments encountered that do not start with '-'

  for (; aIndex < argc; aIndex++) {
    if (argv[aIndex][0] != '-') {
      dashCount++;
    }
    // The first two arguments without a dash (-) at the front should represent the address for the
    // ssh and the requested remote command. The tracing program must thus be inserted between
    // these, if it has been called by the -r flag.

    // TODO: add support for ssh-session without remote command.
    if (dashCount == 2 && remote_riker_path != NULL) {
      // bring in the path to remote-trace within riker and combine with path to riker on remote
      // passed in with -r flag
      const char* remote_detail_path = "share/rkr/\\remote-trace";
      int new_len = strlen(remote_riker_path) - strlen("bin/rkr");
      char usable_path[new_len];
      strcpy(usable_path, (string(remote_riker_path)).substr(0, new_len).c_str());
      char full_path[strlen(usable_path) + strlen(remote_detail_path)];
      strcpy(full_path, usable_path);
      strcat(full_path, remote_detail_path);

      // add argument to run remote-trace
      command[cIndex] = strdup(full_path);
      cIndex++;
      dashCount++;

      // pass the user specified flags for riker for use on the local machine to the remote machine
      if (remote_riker_args != NULL) {
        char option_arg[strlen(remote_riker_args)];
        strcpy(option_arg, remote_riker_args);
        command[cIndex] = strdup(option_arg);
        cIndex++;
      }
    }

    // Copy argument to custom arguments
    command[cIndex] = strdup(argv[aIndex]);
    cIndex++;
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
