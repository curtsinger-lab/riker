#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int main() {
  int fd = open("output", O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if (fd < 0) {
    perror("Failed to open output file");
    return 2;
  }

  char* message = "Hello from C\n";
  if (write(fd, message, strlen(message)) != strlen(message)) {
    perror("Failed to write to output");
    return 2;
  }

  if (close(fd)) {
    perror("Failed to close output file");
    return 2;
  }

  return 0;
}
