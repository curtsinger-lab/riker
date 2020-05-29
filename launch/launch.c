#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define RootBuildCommand "Dodofile"
#define ShellCommand "/bin/sh"

int main() {
  // Check if the buildfile is executable
  if (faccessat(AT_FDCWD, RootBuildCommand, X_OK, AT_EACCESS) == 0) {
    // It is. Directly execute the buildfile
    execl(RootBuildCommand, RootBuildCommand, NULL);
    fprintf(stderr, "Failed to run " RootBuildCommand ": %s\n", strerror(errno));

  } else if (faccessat(AT_FDCWD, RootBuildCommand, R_OK, AT_EACCESS) == 0) {
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
