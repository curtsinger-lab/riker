#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include <bits/stdc++.h>

using std::nullopt;
using std::optional;
using std::string;
using std::vector;

using namespace std;

int main(int argc, char* argv[]) {
  // TODO: Restructure this function to trace without creating a rikerfile (w/ forking into new
  // process maybe)

  // Seperate list of arguments that will be passed to rkr
  char* argv_for_rkr[argc + 10];
  // TODO: Path to riker
  argv_for_rkr[0] = strdup("riker/debug/bin/rkr");
  int argc_for_rkr = 1;

  printf("we run to remote-secondary!\n");
  // open the Rikerfile
  // TODO: Delete newly made Rikerfile at end. Also maybe handle if there is already a rikerfile in
  // this directory.
  std::ofstream outdata;
  outdata.open("/home/furuizhe/Rikerfile");
  if (!outdata) {  // file couldn't be opened
    cerr << "Error: file could not be opened" << endl;
    exit(1);
  }
  outdata << "#!/bin/sh\n\n";

  // iterate through argv arguments to see if user put --show, -log
  // or -o options
  bool thru_channel = false;
  for (int i = 2; i < argc; i++) {
    if (strcmp(argv[i], "--log") == 0 || strcmp(argv[i], "--show") == 0 ||
        strcmp(argv[i], "--show-full") == 0 || strcmp(argv[i], "-o") == 0) {
      thru_channel = true;
    }
  }

  /*
    printf("argv command: ");
    for (int i = 0; i < argc; i++) {
      printf(":%s: ", argv[i]);
    }
    printf("\n");
  */

  int dashCount = 0;
  for (int i = 2; i < argc; i++) {
    // Check if commandline argument is meant for riker or for program started by user
    if (argv[i][0] == '-' && dashCount == 0) {
      if (strcmp(argv[i], "-o") == 0) {
        i++;
        continue;
      }
      argv_for_rkr[argc_for_rkr] = strdup(argv[i]);
      argc_for_rkr++;

      if (strcmp(argv[i], "--log") == 0) {
        i++;
        argv_for_rkr[argc_for_rkr] = strdup(argv[i]);
        argc_for_rkr++;
      }
    } else {
      outdata << argv[i] << " ";
      dashCount++;
    }
  }

  if (thru_channel) {
    argv_for_rkr[argc_for_rkr] = strdup("-o");
    argc_for_rkr++;
    const char* before = "/proc/";
    char command[100];
    const char* after = "/fd/1";
    strcpy(command, before);
    strcat(command, argv[1]);
    strcat(command, after);
    argv_for_rkr[argc_for_rkr] = strdup(command);
    argc_for_rkr++;
  }

  /*
  const char* before = "/proc/";
  char command[100];
  const char* after = "/fd/1";

  strcpy(command, before);
  strcat(command, argv[1]);
  strcat(command, after);
  printf("command: %s\n", command);
  int fdw = open(command, O_WRONLY);
  write(fdw, "Hello\n", 7);
  close(fdw);
  */

  outdata.close();

  argv_for_rkr[argc_for_rkr] = (char*)NULL;

  printf("argv command: ");
  for (int i = 0; i < argc_for_rkr; i++) {
    printf("%s ", argv_for_rkr[i]);
  }
  printf("\n");

  int rc1 = fork();
  if (rc1 == 0) {
    // TODO: ensure we are in the correct directory for riker to be called.
    // TODO: Pass along arguments from local to remote side (particularly --fresh)
    // system("~/riker/debug/bin/rkr --fresh --show");
    execvp("riker/debug/bin/rkr", argv_for_rkr);
  } else {
    // finished executing the secondary remote trace, send a signal to
    // primary remote trace
    wait(NULL);
    // sleep(10);
    const char* before0 = "/proc/";
    char command0[100];
    const char* after0 = "/fd/0";

    strcpy(command0, before0);
    strcat(command0, argv[1]);  // pid of primary remote trace is stored in argv[1]
    strcat(command0, after0);
    // printf("command0: %s\n", command0);
    int fdi = open(command0, O_WRONLY);
    write(fdi, "E", 2);
    close(fdi);
  }
  // std::string command = "cd /proc/" << argv[1] << "/fd";

  for (int j = 0; j < argc_for_rkr; j++) {
    free(argv_for_rkr[j]);
  }

  // system("ls -al");
  return 0;
}