#include <iostream>

#include <string.h>
#include <unistd.h>

#include "util/options.hh"

int main(int argc, char* argv[]) {
  const char* remote_path = getenv("RKR_REMOTE_PATH");

  char* command[argc + 1];  // save space for commands given + trace command

  // TODO: Handle PATH so that we can call the user's ssh instead of slogin
  command[0] = strdup("slogin");
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
    if (dashCount == 2 && remote_path != NULL) {
      // parse in the path to remote-trace
      command[cIndex] = strdup(remote_path);
      strcat(command[cIndex], strdup("/src/ssh-wrapper/\\remote-trace"));
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
  execvp("slogin", command);

  return 0;
}
