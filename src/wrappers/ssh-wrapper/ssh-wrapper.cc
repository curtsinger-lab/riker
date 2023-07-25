#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include <string.h>
#include <unistd.h>

// TODO: make path non-relative
#include <dlfcn.h>
#include <sys/wait.h>

#include "../wrappers/wrapper-utils.hh"
#include "util/options.hh"

using std::nullopt;
using std::optional;
using std::string;
using std::vector;

bool is_clang = false;

typedef int (*execve_fn_t)(const char*, char* const*, char* const*);

/// Make an untraced execve system call.
int execv_untraced(const char* pathname, char* const* argv) {
  // Try to get the untrace execve function from the injected library
  static execve_fn_t _fn = reinterpret_cast<execve_fn_t>(dlsym(RTLD_NEXT, "execve_untraced"));

  // Did we find the injected library function?
  if (!is_clang && _fn) {
    // Yes. Use it.
    return _fn(pathname, argv, environ);

  } else {
    // No. Fall back on a regular execve. This will break some unit tests, but works fine.
    return execve(pathname, argv, environ);
  }
}

/// Make an untraced execvpe system call. Loop through PATH entries to issue each exec.
int execvp_untraced(const char* pathname, char* const* argv) {
  // Does pathname contain a slash character?
  if (strchr(pathname, '/') != NULL) {
    // Yes. Do not search PATH
    execv_untraced(pathname, argv);

  } else {
    // Try PATH entries in order
    for (const auto& entry : path) {
      auto new_pathname = entry + '/' + pathname;
      execv_untraced(new_pathname.c_str(), argv);
    }
  }

  return -1;
}

int main(int argc, char* argv[]) {
  // Combine machine specific called remot
  // If remote path is not set, null will be returned
  // and remote tracing deactivated
  // char* remote_riker_path = getenv("RKR_REMOTE_PATH");

  char* remote_riker_path = "/home/furuizhe/riker";
  char* remote_riker_args = "--fresh --show";
  // char* remote_riker_args = getenv("RKR_REMOTE_ARGS");

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
      // commands[sIndex] = strdup("/home/furuizhe/\\channels-test-remote2");
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
  // execute the command
  printf("Command Primary: ");
  for (int i = 0; i < pIndex; i++) {
    printf("%s ", commandp[i]);
  }
  printf("\n");
  int rc1 = fork();
  if (rc1 < 0) {
    fprintf(stderr, "fork failed\n");
  } else if (rc1 == 0) {
    printf("Local-Primary PID: %d\n", getpid());
    close(fds[0]);                // close reading end in the child
    dup2(fds[1], STDOUT_FILENO);  // send stdout to the pipe
    dup2(fds[1], STDERR_FILENO);  // send stderr to the pipe

    // printf("Here1\n");
    close(fds[1]);  // this descriptor is no longer needed
    // printf("Here2\n");
    execvp_untraced("ssh", commandp);
    // printf("Here3\n");
  } else if (rc1 > 0) {
    close(fds[1]);  // Close writing end of second pipe

    // Read string from child, print it and close
    // reading end.

    char saved_pid[8];
    char strlength[2];
    read(fds[0], strlength, sizeof(char));
    strlength[1] = '\0';
    read(fds[0], saved_pid, atoi(strlength));
    saved_pid[atoi(strlength)] = '\0';
    printf("PID Length: %s\n", strlength);
    printf("Remote Primary PID: %s\n", saved_pid);
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
    printf("Command Secondary: ");
    for (int i = 0; i < sIndex; i++) {
      printf("%s ", commands[i]);
    }
    printf("\n");

    int rc2 = fork();
    if (rc2 < 0) {
      fprintf(stderr, "fork failed\n");
    }
    if (rc2 == 0) {
      printf("Local-Secondary PID: %d\n", getpid());
      sleep(2);
      // TODO: make better method than sleeping to ensure ssh-primary has connected
      execvp("ssh", commands);

    } else {
      sleep(2);
      // char end_sig[10];
      char buffer[2];
      buffer[1] = '\0';
      size_t count;
      printf("Reading from prim\n");
      while ((count = read(fds[0], buffer, sizeof(buffer) - 1)) > 0) {
        printf("%s", buffer);
      }

      close(fds[0]);
      wait(NULL);
      printf("Done?\n");
    }
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
