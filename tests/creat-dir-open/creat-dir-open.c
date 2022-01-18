#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

int main() {
  int fd = open("outcome", O_CREAT | O_DIRECTORY, S_IRWXU | S_IRWXG);
  return 0;
}
