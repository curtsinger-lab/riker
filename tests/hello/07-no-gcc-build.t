This test runs a build by directly invoking compiler toolchain commands.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo hello

Copy in the basic Dodofile and make sure it's executable
  $ cp no-gcc-Dodofile Dodofile
  $ chmod u+x Dodofile

Set up the original source file
  $ cp file_versions/hello-original.c hello.c

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  cpp hello.c hello.i
  [^ ]*cc1 .* (re)
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Run the hello executable
  $ ./hello
  Hello world

Run a rebuild, which should do nothing.
  $ $DODO --show

Make sure the hello executable still works
  $ ./hello
  Hello world

Clean up
  $ rm -rf .dodo hello hello.i hello.o hello.s Dodofile
