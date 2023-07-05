#include <iostream>

#include "util/options.hh"

int main(int argc, char** argv) {
  // Can also use /usr/bin/ssh if slogin is not available
  // TODO: auto path to scp
  std::string commandbuild = "/usr/bin/scp";

  // Use option -S to use riker's ssh instead of scp's auto use of system ssh
  commandbuild = commandbuild + " -S ssh";

  // TODO: right now it only accepts one command line argument
  for (int i = 1; i < argc; ++i) commandbuild = commandbuild + " " + argv[i];

  const char* command = commandbuild.c_str();
  system(command);

  return 0;
}
