This test runs a simple gcc build, changes an input file, and verifies that the rebuild updates the output.

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
  dodo-launch Dodofile
  Dodofile
  cpp hello.c hello.i
  cc1 -E .* (re)
  cc1 .* (re)
  as .* (re)
  collect2 .* (re)
  ld .* (re)

Run the hello executable
  $ ./hello
  Hello world

Modify the one source file
  $ cp file_versions/hello-modified.c hello.c

Run a rebuild, which should rerun cc1, as, and ld
  $ $DODO --show
  cc1 -E .* (re)
  cc1 .* (re)
  as .* (re)
  collect2 .* (re)
  ld .* (re)

Make sure the hello executable is updated
  $ ./hello
  Goodbye world

Run an additional rebuild, which should now do no work
  $ $DODO --show

Clean up
  $ rm -rf .dodo hello hello.i hello.o hello.s Dodofile
  $ cp file_versions/hello-original.c hello.c
