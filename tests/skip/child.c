#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main() {
  int input_fd = open("child_input", O_RDONLY);
  int output_fd = open("child_output", O_WRONLY | O_CREAT | O_TRUNC, 0644);

  char buffer[512];
  ssize_t bytes = read(input_fd, buffer, 512);
  write(output_fd, buffer, bytes);

  close(input_fd);
  close(output_fd);

  return 0;
}
