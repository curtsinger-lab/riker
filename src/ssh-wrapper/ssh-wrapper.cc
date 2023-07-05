#include <iostream>

#include <string.h>
#include <unistd.h>

#include "util/options.hh"

int main(int argc, char* argv[]) {
  char* command[argc + 10];  // manually set space to +10
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

    // if we already have 2 argument that doesn't start with -,
    // Then it is the place to parse the path to remote-trace
    if (dashCount == 2) {
      // parse in the path to remote-trace
      command[cIndex] = strdup(getenv("RKR_REMOTE_PATH"));
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
