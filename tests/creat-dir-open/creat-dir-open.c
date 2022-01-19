#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

int main() {
  int fd = openat(AT_FDCWD, "/workspaces/riker/tests/creat-dir-open/outcome",
                  O_RDWR | O_CREAT | O_DIRECTORY, S_IRWXU | S_IRWXG);
  return 0;
}
