#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define RootBuildCommand "Dodofile"
#define ShellCommand "/bin/sh"

int main() {
  // First, try to execute the root build file
  execl(RootBuildCommand, RootBuildCommand, NULL);

  // If we reach this point, the buildfile was not executable. Is it readable?
  if (faccessat(AT_FDCWD, RootBuildCommand, R_OK, AT_EACCESS) == 0) {
    // The buildfile is not executable, but we have read access. Run it with /bin/sh
    execl(ShellCommand, ShellCommand, RootBuildCommand, NULL);
    fprintf(stderr, "Failed to run " RootBuildCommand " with shell " ShellCommand ": %s\n",
            strerror(errno));

  } else {
    // The buildfile is neither executable nor readable. This won't work.
    fprintf(stderr, "Unable to access " RootBuildCommand ".\n");
    fprintf(stderr, "  This file must be directly executable or runnable with " ShellCommand ".\n");
  }

  return 2;
}
