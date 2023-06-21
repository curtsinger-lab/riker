#include <iostream>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int agrc, char* argv[]) {
  int rc = fork();
  if (rc < 0) {
    fprintf(stderr, "fork failed\n");
  } else if (rc == 0) {
    // Build the commands sent to run over ssh connection
    std::string commandbuild = "";
    for (int i = 1; i < agrc; i++) {
      commandbuild = commandbuild + argv[i] + " ";
    }

    const char* command = commandbuild.c_str();
    system(command);

  } else {
    int rc_wait = wait(NULL);
    // TODO: Add trace to parent
    if (rc_wait == -1) {
      fprintf(stderr, "fork failed\n");
    }
  }

  return 0;
}