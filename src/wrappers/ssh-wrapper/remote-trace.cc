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

  // Build the commands sent to run over ssh connection
  char* command[argc + 10];

  // copy the comment to the array
  for (int i = 1; i < argc; i++) {
    command[i] = strdup(argv[i]);
  }

  std::ofstream trialData;
  trialData.open("info");
  trialData << "ARGUMENTS:  ";
  for (int i = 0; i < argc; i++) {
    trialData << command[i] << " ";
  }
  trialData << "\n";
  trialData.close();

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
  for (int i = 1; i < argc; i++) {
    outdata << command[i] << " ";
  }

  outdata.close();

  // TODO: ensure we are in the correct directory for riker to be called.
  // TODO: Pass along arguments from local to remote side (particularly --fresh)
  system("~/riker/debug/bin/rkr --fresh --show");
  return 0;
}
