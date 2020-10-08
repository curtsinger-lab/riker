#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>

int main() {
  int dirfd = AT_FDCWD;
  char *path = "mydir";
  int flags = O_RDONLY|O_CLOEXEC|O_NOFOLLOW;
  int mode = 0;
  printf("Trying openat(%d, \"%s\", %d, %d)\n", dirfd, path, flags, mode);
  int fd = openat(dirfd, path, flags, mode);
  printf("Got fd = %d\n", fd);
  return 0;
}
