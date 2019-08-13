#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char** argv) {
  int fd = open(argv[1], O_RDWR);
  lseek(fd, 0, SEEK_END);
  write(fd, "A", 1);

  return 0;
}

