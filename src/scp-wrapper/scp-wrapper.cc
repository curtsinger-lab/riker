#include <iostream>

#include "util/options.hh"

int main(int argc, char** argv) {
  // Can also use /usr/bin/ssh if slogin is not available
  std::string commandbuild = "/usr/bin/scp";

  commandbuild = commandbuild + " -S ssh";

  // right now it only accept only one command line argument
  for (int i = 1; i < argc; ++i) commandbuild = commandbuild + " " + argv[i];

  const char* command = commandbuild.c_str();
  system(command);

  return 0;
}
