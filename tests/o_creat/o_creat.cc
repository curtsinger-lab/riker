#include <iostream>

#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int main() {
  // Open an input file
  int input_fd = open("input", O_RDONLY);
  if (input_fd < 0) {
    std::cout << "Failed to open input file: " << strerror(errno) << std::endl;
    exit(2);
  }

  // Read the input file. This allows us to trigger a rebuild by editing the file
  char buf[128];
  read(input_fd, buf, 128);

  // create a file
  int fd = openat(AT_FDCWD, "foo", O_WRONLY | O_CREAT | O_EXCL, 0444);

  if (fd == -1) {
    // fail
    std::cout << "Error: " << strerror(errno) << std::endl;
  } else {
    // write to it
    std::cout << "File created with fd = " << fd << std::endl;

    const char* buf = "Hello world!\n";
    write(fd, buf, strlen(buf));
  }
}
