#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define ShellCommand "/bin/sh"

int main(int argc, char** argv) {
  // Make sure a build file was provided as an argument
  if (argc != 2) {
    fprintf(stderr, "%s was invoked without a path to the root build file.\n", argv[0]);
    return 2;
  }

  // Get the build file argument
  const char* buildfile = argv[1];

  // First, try to execute the root build file
  execl(buildfile, buildfile, NULL);

  // If we reach this point, the buildfile was not executable. Is it readable?
  if (faccessat(AT_FDCWD, buildfile, R_OK, AT_EACCESS) == 0) {
    // The buildfile is not executable, but we have read access. Run it with /bin/sh
    execl(ShellCommand, ShellCommand, buildfile, NULL);
    // fprintf(stderr, "Failed to run %s with shell " ShellCommand ": %s\n", cmd, strerror(errno));
  }

  // Looks like those did not work
  fprintf(stderr, "Unable to launch build file %s.\n", buildfile);
  fprintf(stderr,
          "  Write build steps in a file named `%s`.\n"
          "  This file must be either directly executable, or runnable with `" ShellCommand "`.\n",
          buildfile);

  return 2;
}
