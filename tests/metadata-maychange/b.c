#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

int main() {
  // Open and read from b_input
  int fd = open("b_input", O_RDONLY);
  char buf[128];
  read(fd, buf, 128);
  close(fd);

  return 0;
}
