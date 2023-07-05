#include <iostream>
#include <string.h>
#include <unistd.h>

#include "util/options.hh"

int main(int argc, char* argv[]) {
	// Can also use /usr/bin/ssh if slogin is not available
	// std::string commandbuild = "slogin";
	
	std::cout << "\n";
	//printf("argc: %d\n", argc);
	
	char* command[argc + 10];  
	command[0] = strdup("slogin");
	int cIndex = 1;
	int aIndex = 1;
	int dashCount = 0;
	
	// std::cout << "The command is following: " << command[0] << "\n"; 
	
	for (; aIndex < argc; aIndex++)  {  
		if (argv[aIndex][0] != '-') {
			dashCount++;
		}
		
		if (dashCount == 2) {
			command[cIndex] = strdup(getenv("RKR_REMOTE_PATH"));
			strcat(command[cIndex], strdup("/src/ssh-wrapper/\\remote-trace"));
			cIndex++;
			command[cIndex] = strdup(argv[aIndex]);
			cIndex++;
			dashCount++;
		}
		else {
			command[cIndex] = strdup(argv[aIndex]);
			cIndex++;
		}
	}  		
  	
  	
  	
  	/*
	// right now it only accept only one command line argument
	for (int i = 1; i < argc - 1; ++i) 
		commandbuild = commandbuild + " " + argv[i];
	*/
	
	/*
	command[index] = strdup(getenv("RKR_REMOTE_PATH"));
	strcat(command[index], strdup("/src/ssh-wrapper/\\remote-trace"));
	
	// printf("%s\n", argv[argc - 1]);
	
	command[index + 1] = strdup(argv[argc - 1]);
	*/
	
	command[cIndex] = (char*)NULL;
	
	printf("cIndex: %d; argc: %d\n", cIndex, argc);
	
	std::cout << "This is the command: ";
	for (int i = 0; i < cIndex + 1; i++) {
		std::cout << command[i] << " "; 
	}
	std::cout << "\n";

	
	execvp("slogin", command);
	
	
	
	
	/*
	commandbuild = commandbuild + " " + getenv("RKR_REMOTE_PATH") + "/src/ssh-wrapper/\\remote-trace";
	

	commandbuild = commandbuild + " " + argv[argc - 1];
	const char* command = commandbuild.c_str();
	system(command);
	*/

	return 0;
}
