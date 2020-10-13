
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
  char *path = "a_file";
  struct stat statbuf;
  int rv = stat(path, &statbuf);
  if (rv) {
    printf("'%s' does not exist.\n", path);
    int fd = open(path, O_WRONLY|O_CREAT|O_EXCL, S_IRWXU|S_IRWXG|S_IROTH);
    write(fd, "hello\n", 6);
  } else {
    printf("'%s' exists.\n", path);
    int fd = open(path, O_RDWR|O_APPEND);
    write(fd, "world\n", 6);
  }
  
  return 0;
}
