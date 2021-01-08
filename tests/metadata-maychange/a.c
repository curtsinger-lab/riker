#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

int main() {
  // Open and read from a_input
  int fd = open("a_input", O_RDONLY);
  char buf[128];
  read(fd, buf, 128);
  close(fd);

  // Now change metadata on b_input
  chmod("b_input", 0644);

  return 0;
}
