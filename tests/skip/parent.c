#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {
  int input_fd = open("parent_input", O_RDONLY);
  int output_fd = open("parent_output", O_WRONLY | O_CREAT | O_TRUNC, 0644);

  char buffer[512];
  ssize_t bytes = read(input_fd, buffer, 512);

  write(output_fd, buffer, bytes);

  int child_id = fork();
  if (child_id == 0) {
    execl("./child", "./child", NULL);
  } else {
    wait(NULL);
  }

  close(input_fd);
  close(output_fd);

  return 0;
}