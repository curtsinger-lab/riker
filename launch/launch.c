#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define Buildfile "Dodofile"
#define ShellCommand "/bin/sh"

int main(int argc, char** argv) {
  // First, try to execute the root build file
  char* argv2[argc];
  argv2[0] = Buildfile;
  for (int i = 1; i <= argc; i++) {
    argv2[i + 1] = argv[i];
  }
  execv(Buildfile, argv2);

  // If we reach this point, the buildfile was not executable. Is it readable?
  if (faccessat(AT_FDCWD, Buildfile, R_OK, AT_EACCESS) == 0) {
    // The buildfile is not executable, but we have read access. Run it with /bin/sh
    execl(ShellCommand, ShellCommand, Buildfile, NULL);
    perror("Failed to run " Buildfile " with shell " ShellCommand);
    exit(2);
  }

  // At this point, we know there must not be a usable build file. Can we import from a make?
  if (faccessat(AT_FDCWD, "GNUmakefile", R_OK, AT_EACCESS) == 0 ||
      faccessat(AT_FDCWD, "makefile", R_OK, AT_EACCESS) == 0 ||
      faccessat(AT_FDCWD, "Makefile", R_OK, AT_EACCESS) == 0) {
    // Run make
    execlp("make", "make", "--always-make", "--quiet", NULL);
  }

  // Looks like those did not work
  fprintf(stderr, "Unable to launch build file %s.\n", Buildfile);
  fprintf(stderr,
          "  Write build steps in a file named `%s`.\n"
          "  This file must be either directly executable, or runnable with `" ShellCommand "`.\n",
          Buildfile);

  return 2;
}
