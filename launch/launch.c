#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define RootBuildCommand "Dodofile"
#define MAKEFILES \
  { "GNUmakefile", "makefile", "Makefile" }
#define ShellCommand "/bin/sh"
#define MakeCommand "/usr/bin/make"

int main() {
  // First, try to execute the root build file
  execl(RootBuildCommand, RootBuildCommand, NULL);

  // If we reach this point, the buildfile was not executable. Is it readable?
  if (faccessat(AT_FDCWD, RootBuildCommand, R_OK, AT_EACCESS) == 0) {
    // The buildfile is not executable, but we have read access. Run it with /bin/sh
    execl(ShellCommand, ShellCommand, RootBuildCommand, NULL);
    // fprintf(stderr, "Failed to run %s with shell " ShellCommand ": %s\n", cmd, strerror(errno));
  } else {
    // The Dodofile is either unreadable or it doesn't exist at all.
    // Is there a readable Makefile available?
    const char* makefiles[] = MAKEFILES;
    for (int i = 0; i < sizeof(makefiles) / sizeof(makefiles[0]); i++) {
      if (faccessat(AT_FDCWD, makefiles[i], R_OK, AT_EACCESS) == 0) {
        // a successful call will not return
        execl(MakeCommand, MakeCommand, NULL);
      }
    }
  }

  // Looks like those did not work
  fprintf(stderr, "Unable to find either " RootBuildCommand " or Makefile.\n");
  fprintf(stderr, "  A " RootBuildCommand
                  " file should be present and be either directly"
                  " executable or runnable with " ShellCommand ".\n");

  return 2;
}
