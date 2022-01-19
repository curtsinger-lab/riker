This test runs a build by directly invoking compiler toolchain commands.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr hello hello.i hello.s hello.o

Copy in the basic Rikerfile and make sure it's executable
  $ cp no-gcc-Rikerfile Rikerfile
  $ chmod u+x Rikerfile

Set up the original source file
  $ cp file_versions/hello-original.c hello.c

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  cpp .* (re)
  [^ ]*cc1 -E .* (re)
  gcc .* (re)
  [^ ]*cc1 .* (re)
  gcc .* (re)
  [^ ]*as .* (re)
  gcc .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Run the hello executable
  $ ./hello
  Hello world

Run a rebuild, which should do nothing.
  $ rkr --show

Make sure the hello executable still works
  $ ./hello
  Hello world

Clean up
  $ rm -rf .rkr hello hello.i hello.o hello.s Rikerfile
