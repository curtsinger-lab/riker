#include <iostream>

#include "util/options.hh"

int main(int argc, char** argv) {
  // Hacky way to ensure correct ssh is used, need to ensure this is where user's ssh is installed
  std::string commandbuild = "/usr/bin/ssh";
  for (int i = 1; i < argc - 1; ++i) commandbuild = commandbuild + " " + argv[i];

  commandbuild = commandbuild + " " + getenv("RKR_REMOTE_PATH") + "/src/ssh-wrapper/\\remote-trace";

  commandbuild = commandbuild + " " + argv[argc - 1];
  const char* command = commandbuild.c_str();
  system(command);

  return 0;
}