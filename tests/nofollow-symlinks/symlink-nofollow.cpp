#include <iostream>

#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

using namespace std;

int main() {
  // we want to make sure that riker has actually seen the symlink
  int rv = symlink("a_file", "a_symlink");
  int fd = openat(AT_FDCWD, "a_symlink", O_NOFOLLOW);

  cout << "fd = " << fd << endl;
  if (fd == -1) {
    cout << "Error: " << strerror(errno) << endl;
  }
}
