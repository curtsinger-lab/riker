This test runs a wildcard gcc build and verifies that a rebuild does no work.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo hello

Copy in the basic Dodofile and make sure it's executable
  $ cp wildcard-Dodofile Dodofile
  $ chmod u+x Dodofile

Set up the original source file
  $ cp file_versions/hello-original.c hello.c

Run the build
  $ $DODO --show
  dodo-launch Dodofile
  Dodofile
  gcc -o hello hello.c
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
  $ rm -rf .dodo hello Dodofile
