#include <string>
#include <thread>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

using std::string;

// Execute the Makefile (with -n) and redirect output to Rikerfile
void getMakeCommands(string makefile_path) {
  string rikerfile_path = "./Rikerfile";
  int out_fd = open("Rikerfile", O_WRONLY | O_CREAT | O_TRUNC, 0644);
  char buffer[512];
  // Create a new thread to execute make -n
  string command = "make -n | " + rikerfile_path;
  if (fork() == 0) {
    if (execv(makefile_path.c_str(), command.c_str())) } else {
  }
}

int main() {
  string makefile_path = "./Makefile";
}