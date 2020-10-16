#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

int main() {
  int fd = open("outcome", O_CREAT|O_DIRECTORY, S_IRWXU|S_IRWXG);
  if (fd == -1) {
    // we should never see this output in Linux
    printf("Invalid syscall: %s\n", strerror(errno)); 
  }
  return 0;
}
