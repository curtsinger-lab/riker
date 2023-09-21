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

  init_path();

  // Can also use /usr/bin/ssh if slogin is not available
  char* command[argc + 4];
  int index = 1;

  command[0] = strdup("scp");

  string pathbuild = riker_path + "/ssh";
  // Use option -S to use riker's ssh instead of scp's auto use of system ssh
  if (getenv("RKR_REMOTE_PATH") != NULL) {
    command[1] = strdup("-S");
    command[2] = strdup(pathbuild.c_str());
    index += 2;
  }
  // TODO: Allow multiple command line arguments for scp
  for (int i = 1; i < argc; ++i) {
    command[index] = strdup(argv[i]);
    index++;
  }
  command[index] = (char*)NULL;
  execvp("scp", command);

  printf("here\n");
  for (int i = 0; i < index; i++) {
    free(command[i]);
  }

  return 0;
}
