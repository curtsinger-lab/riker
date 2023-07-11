#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main() {
  int input_fd = open("input", O_RDONLY);
  if (input_fd < 0) {
    perror("Failed to open input file");
    exit(EXIT_FAILURE);
  }

  int output_fd = open("output", O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if (output_fd < 0) {
    perror("Failed to open output file");
    exit(EXIT_FAILURE);
  }

  char buffer[256];
  snprintf(buffer, 256, "/proc/self/fd/%d", input_fd);
  int indirect_input_fd = open(buffer, O_RDONLY);
  if (indirect_input_fd < 0) {
    perror("Failed to reopen input file via /proc/self/fd");
    exit(EXIT_FAILURE);
  }

  char data[256];
  ssize_t bytes_read;
  if ((bytes_read = read(indirect_input_fd, data, 256)) <= 0) {
    perror("Failed to read input data");
    exit(EXIT_FAILURE);
  }

  if (write(output_fd, data, bytes_read) != bytes_read) {
    perror("Failed to write data to output file");
    exit(EXIT_FAILURE);
  }

  return 0;
}
