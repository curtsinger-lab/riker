#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include <string.h>
#include <unistd.h>

// TODO: make path non-relative
#include <sys/wait.h>

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

  int fds[2];

  if (pipe(fds) == -1) {
    fprintf(stderr, "Pipe Failed");
    return 1;
  }

  init_path();

  char* commandp[argc + 10];  // save space for commands given for primary
  char* commands[argc + 10];  // save space for commands given + trace command in secondary

  commandp[0] = strdup("ssh");
  commands[0] = strdup("ssh");

  int pIndex = 1;  // index for custom arguments for primary
  int sIndex = 1;  // index for custom arguments for secondary
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
      // add argument to run remote-trace-primary
      commandp[pIndex] = strdup("-oControlMaster=yes");
      pIndex++;

      // TODO: Generalize
      commandp[pIndex] = strdup("-oControlPath=~/.ssh/%r@%h:%p");
      pIndex++;
      commands[sIndex] = strdup("-oControlPath=~/.ssh/%r@%h:%p");
      sIndex++;

      // bring in the path to remote-trace within riker and combine with path to riker on remote
      const char* remote_detail_path_secondary = "/debug/share/rkr/\\remote-secondary";
      char full_path_secondary[strlen(remote_riker_path) + strlen(remote_detail_path_secondary)];
      strcpy(full_path_secondary, remote_riker_path);
      strcat(full_path_secondary, remote_detail_path_secondary);

      const char* remote_detail_path_primary = "/debug/share/rkr/\\remote-primary";
      char full_path_primary[strlen(remote_riker_path) + strlen(remote_detail_path_primary)];
      strcpy(full_path_primary, remote_riker_path);
      strcat(full_path_primary, remote_detail_path_primary);

      commandp[pIndex] = strdup(full_path_primary);
      commands[sIndex] = strdup(full_path_secondary);

      pIndex++;
      sIndex++;
      break;
    }
    // parse argument normally
    else {
      commandp[pIndex] = strdup(argv[aIndex]);
      commands[sIndex] = strdup(argv[aIndex]);
      pIndex++;
      sIndex++;
    }
  }

  // end the array with null
  commandp[pIndex] = (char*)NULL;

  /*
  printf("command:\n");
  for (int i = 0; i < pIndex; i++) {
    printf("%s ", commandp[i]);
  }
  printf("\n");
  */
  /*
    std::cout << "commands: ";
    for (int i = 0; i < sIndex; i++) {
      std::cout << commands[i] << " ";
    }
    std::cout << "\n";

    std::cout << "commandp: ";

    for (int i = 0; i < pIndex; i++) {
      std::cout << commandp[i] << " ";
    }
    std::cout << "\n";
    */

  // execute the command
  int rc1 = fork();
  if (rc1 < 0) {
    fprintf(stderr, "fork failed\n");
  } else if (rc1 == 0) {
    close(fds[0]);  // close reading end in the child

    /*
    printf("Command1:\n");
    for (int i = 0; i < pIndex; i++) {
      printf("%s ", commandp[i]);
    }
    printf("\n");
    */

    dup2(fds[1], 1);  // send stdout to the pipe
    dup2(fds[1], 2);  // send stderr to the pipe

    close(fds[1]);  // this descriptor is no longer needed
    execvp("ssh", commandp);
  } else if (rc1 > 0) {
    sleep(5);
    char saved_pid[8];

    close(fds[1]);  // Close writing end of second pipe

    // Read string from child, print it and close
    // reading end.
    char strlength[2];
    read(fds[0], strlength, sizeof(char));
    strlength[1] = '\0';
    read(fds[0], saved_pid, atoi(strlength));
    saved_pid[atoi(strlength)] = '\0';
    printf("Length: %s\n", strlength);
    printf("Sent data: %s\n", saved_pid);
    commands[sIndex] = strdup(saved_pid);
    sIndex++;
    if (remote_riker_args != NULL) {
      char option_arg[strlen(remote_riker_args)];
      strcpy(option_arg, remote_riker_args);

      // add argument to run remote-trace
      commands[sIndex] = strdup(option_arg);
      sIndex++;
    }
    for (; aIndex < argc; aIndex++) {
      commands[sIndex] = strdup(argv[aIndex]);
      sIndex++;
    }
    commands[sIndex] = (char*)NULL;

    /*
    printf("Commands:\n");
    for (int i = 0; i < sIndex; i++) {
      printf("%s ", commands[i]);
    }
    printf("\n");
    */

    int rc2 = fork();
    if (rc2 == 0) {
      // TODO: make better method than sleeping to ensure ssh-primary has connected
      printf("We run to here!\n");
      sleep(3);
      execvp("ssh", commands);
    }
    sleep(10);
    close(fds[0]);
    // wait(NULL);
  }
  // TODO: Figure out how tf memory gonna work here. maybe this will actually work bc everyone has
  // their own index values?
  for (int i = 0; i < pIndex; i++) {
    free(commandp[i]);
  }

  for (int i = 0; i < sIndex; i++) {
    free(commands[i]);
  }
  return 0;
}
