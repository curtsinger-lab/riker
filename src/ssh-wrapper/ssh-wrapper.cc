#include <iostream>

#include "util/options.hh"

int main(int argc, char** argv) {
  // Can also use /usr/bin/ssh if slogin is not available
  std::string commandbuild = "slogin"; 		
  
  // right now it only accept only one command line argument
  for (int i = 1; i < argc - 1; ++i) 
  	commandbuild = commandbuild + " " + argv[i];

  commandbuild = commandbuild + " " + getenv("RKR_REMOTE_PATH") + "/src/ssh-wrapper/\\remote-trace";

  commandbuild = commandbuild + " " + argv[argc - 1];
  const char* command = commandbuild.c_str();
  system(command);

  return 0;
}
