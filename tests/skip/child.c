#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main() {
  int output_fd = open("child_output", O_WRONLY | O_CREAT | O_TRUNC, 0644);
  char* buffer = "child\n";
  write(output_fd, buffer, 6);

  return 0;
}
