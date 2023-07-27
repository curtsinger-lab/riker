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

  // open the Rikerfile and write command to it
  // TODO: Delete newly made Rikerfile at end. Also maybe handle if there is already a rikerfile in
  // this directory.
  std::ofstream outdata;
  outdata.open("Rikerfile");
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

  int dashCount = 0;
  // iterate through command line argument
  for (int i = 2; i < argc; i++) {
    // Check if commandline argument is meant for riker or for program started by user
    if (argv[i][0] == '-' && dashCount == 0) {
      if (strcmp(argv[i], "-o") == 0) {
        i++;
        continue;
      }
      argv_for_rkr[argc_for_rkr] = strdup(argv[i]);
      argc_for_rkr++;

      // if the command argument if --log, skip it
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

  // if user put --show, -log, or -o operations, we need to send command information
  // back to primary channel
  if (thru_channel) {
    // put -o to generate log file
    argv_for_rkr[argc_for_rkr] = strdup("-o");
    argc_for_rkr++;

    // create string to write to
    const char* before = "/proc/";
    const char* after = "/fd/1";
    char command[strlen(before) + strlen(after) + strlen(argv[1]) + 1];
    strcpy(command, before);
    strcat(command, argv[1]);
    strcat(command, after);

    // append command for executable
    argv_for_rkr[argc_for_rkr] = strdup(command);
    argc_for_rkr++;
  }

  outdata.close();

  argv_for_rkr[argc_for_rkr] = (char*)NULL;

  int rc1 = fork();
  // create child process to execute the command
  if (rc1 == 0) {
    // TODO: ensure we are in the correct directory for riker to be called.
    // TODO: Pass along arguments from local to remote side (particularly --fresh)
    execvp("riker/debug/bin/rkr", argv_for_rkr);
  } else {
    // finished executing the secondary remote trace, send a signal to
    // primary remote trace
    wait(NULL);
    const char* before0 = "/proc/";
    const char* after0 = "/fd/0";
    char command0[strlen(before0) + strlen(after0) + strlen(argv[1]) + 1];

    strcpy(command0, before0);
    strcat(command0, argv[1]);  // pid of primary remote trace is stored in argv[1]
    strcat(command0, after0);

    // send finish signal to primary channel
    int fdi = open(command0, O_WRONLY);
    write(fdi, "E", 2);
    close(fdi);
  }

  // free memory
  for (int j = 0; j < argc_for_rkr; j++) {
    free(argv_for_rkr[j]);
  }

  // system("ls -al");
  return 0;
}