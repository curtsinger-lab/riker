This test runs a simple gcc build and verifies that a rebuild does no work.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr hello

Copy in the basic Rikerfile and make sure it's executable
  $ cp basic-Rikerfile Rikerfile
  $ chmod u+x Rikerfile

Set up the original source file
  $ cp file_versions/hello-original.c hello.c

Run the build
  $ rkr --show --no-wrapper
  rkr-launch
  Rikerfile
  gcc -o hello hello.c
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
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
  $ rm -rf .rkr hello Rikerfile
