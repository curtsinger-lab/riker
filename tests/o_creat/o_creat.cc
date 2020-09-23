#include <iostream>

#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int main() {
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
