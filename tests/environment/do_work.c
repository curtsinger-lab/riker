#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main() {
  // TODO: add error checking
  // Open the input file
  int in_fd = open("input", O_RDONLY);

  // Read from the input file
  char buffer[512];
  ssize_t bytes_read = read(in_fd, buffer, 512);

  // Close the input file
  close(in_fd);

  // Create an output file
  int out_fd = open("output", O_WRONLY | O_CREAT | O_TRUNC, 0644);

  // Copy data read from input to output
  write(out_fd, buffer, bytes_read);

  // Try to get the value of the EXTRA_MESSAGE environment variable
  char* extra_message = getenv("EXTRA_MESSAGE");
  char* extra_extra_message = getenv("EXTRA_EXTRA_MESSAGE");

  // If there is an extra message, write it to the output
  if (extra_message != NULL) {
    write(out_fd, extra_message, strlen(extra_message));
    write(out_fd, "\n", 1);
  }
  // If there is an extra message, write it to the output
  if (extra_extra_message != NULL) {
    write(out_fd, extra_extra_message, strlen(extra_extra_message));
    write(out_fd, "\n", 1);
  }

  // Close the output file
  close(out_fd);

  return 0;
}
