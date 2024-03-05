#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define Buildfile "Rikerfile"
#define ShellCommand "/bin/sh"

int main(int argc, char** argv) {
    // If multiple arguments are passed to launch, which is the case for the "run" subcommand
  if (argc > 1) {
    // First, build a new argv array for the shell command
    char* new_argv[argc + 2];
    new_argv[0] = ShellCommand;

    // Copy the original arguments, including the NULL terminator at argv[argc]
    for (int i = 2; i <= argc; i++) {
      new_argv[i] = argv[i - 1];
    }
    new_argv[1] = "-c";

    execv(ShellCommand, new_argv);

    return 0;
  }

  // First, try to execute the root build file
  argv[0] = Buildfile;
  execv(Buildfile, argv);

  // If we reach this point, the buildfile was not executable. Is it readable?
  if (faccessat(AT_FDCWD, Buildfile, R_OK, AT_EACCESS) == 0) {
    // The buildfile is not executable, but we have read access. Run it with /bin/sh

    // First, build a new argv array that starts with /bin/sh
    char* new_argv[argc + 2];
    new_argv[0] = ShellCommand;

    // Copy the original arguments, including the NULL terminator at argv[argc]
    for (int i = 0; i <= argc; i++) {
      new_argv[i + 1] = argv[i];
    }

    execv(ShellCommand, new_argv);
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
