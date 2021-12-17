This test runs a simple gcc build, changes an input file, and verifies that the rebuild updates the output.

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

Modify the one source file
  $ cp file_versions/hello-modified.c hello.c

Run a rebuild, which should rerun cc1, as, and ld
  $ rkr --show --no-wrapper
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*ld .* (re)

Make sure the hello executable is updated
  $ ./hello
  Goodbye world

Run an additional rebuild, which should now do no work
  $ rkr --show

Clean up
  $ rm -rf .rkr hello Rikerfile
  $ cp file_versions/hello-original.c hello.c
