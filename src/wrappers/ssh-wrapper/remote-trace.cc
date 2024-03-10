#include <iostream>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

using std::cerr;
using std::endl;
#include <fstream>
using std::ofstream;
#include <cstdlib>  // for exit function

int main(int argc, char* argv[]) {
  // TODO: Restructure this function to trace without creating a rikerfile (w/ forking into new
  // process maybe)

  // Seperate list of arguments that will be passed to rkr
  char* argv_for_rkr[argc];
  // TODO: Path to riker
  argv_for_rkr[0] = strdup("riker/debug/bin/rkr");
  int argc_for_rkr = 1;

  // open the Rikerfile
  // TODO: Delete newly made Rikerfile at end. Also maybe handle if there is already a rikerfile in
  // this directory.
  std::ofstream outdata;
  outdata.open("Rikerfile");
  if (!outdata) {  // file couldn't be opened
    cerr << "Error: file could not be opened" << endl;
    exit(1);
  }

  // write command to the Rikerfile
  outdata << "#!/bin/sh\n\n";
  int dashCount = 0;
  for (int i = 1; i < argc; i++) {
    // Check if commandline argument is meant for riker or for program started by user
    if (argv[i][0] == '-' && dashCount == 0) {
      // NOTE/TODO: calling programs such as scp that use the whole channel, along with flags that
      // print output currently will break the command. Fix is in place for next update.
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

  outdata.close();

  argv_for_rkr[argc_for_rkr] = (char*)NULL;

  // TODO: ensure we are in the correct directory for riker to be called.
  // TODO: Pass along arguments from local to remote side (particularly --fresh)
  // system("~/riker/debug/bin/rkr --fresh --show");
  execvp("riker/debug/bin/rkr", argv_for_rkr);

  for (int j = 0; j < argc_for_rkr; j++) {
    free(argv_for_rkr[j]);
  }

  return 0;
}
