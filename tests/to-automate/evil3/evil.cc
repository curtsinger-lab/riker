#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <vector>

int main(int argc, char** argv) {
  int fd = open(argv[1], O_RDWR);

  std::vector<char> chars;

  char c;
  while(read(fd, &c, 1) == 1) {
    chars.push_back(c);
  }

  lseek(fd, 0, SEEK_SET);

  chars.push_back('A');

  for(auto c: chars) {
    write(fd, &c, 1);
  }

  close(fd);

  return 0;
}

