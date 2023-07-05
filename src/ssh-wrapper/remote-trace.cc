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
#include <cstdlib> // for exit function

int main(int argc, char* argv[]) {
	int rc = fork();
	if (rc < 0) {
		fprintf(stderr, "fork failed\n");
	} else if (rc == 0) {  	
		// Build the commands sent to run over ssh connection
		char* command[argc + 10];
		
		// copy the comment to the array
		for (int i = 1; i < argc; i++) {
			command[i] = strdup(argv[i]);
		}


		// open the Rikerfile		
		std::ofstream outdata;
		outdata.open("Rikerfile");
		if( !outdata ) { // file couldn't be opened
			cerr << "Error: file could not be opened" << endl;
			exit(1);
	   	}
	   	
	   	// write command to the Rikerfile
	   	outdata << "#!/bin/sh\n\n";
	   	for (int i = 1; i < argc; i++) {
			outdata << command[i] << " ";
		}
	   	
	   	outdata.close();
		
		// TODO: change to the directory where riker is in
		system("~/riker/debug/bin/rkr --fresh");
		system("rkr graph");

	} else {
		int rc_wait = wait(NULL);
		// TODO: Add trace to parent
		if (rc_wait == -1) {
		  fprintf(stderr, "fork failed\n");
		}
	}

	return 0;
}
