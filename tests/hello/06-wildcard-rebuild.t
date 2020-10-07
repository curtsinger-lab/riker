This test runs a wildcard gcc build, changes an input file, and verifies that the rebuild updates the output.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo hello foo

Copy in the basic Dodofile and make sure it's executable
  $ cp wildcard-Dodofile Dodofile
  $ chmod u+x Dodofile

Set up the original source file
  $ cp file_versions/hello-original.c hello.c

Touch the output file
  $ touch hello

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

Modify the one source file
  $ cp file_versions/hello-modified.c hello.c

Run a rebuild, which should rerun cc1, as, and ld
  $ $DODO --show
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*ld .* (re)

Make sure the hello executable is updated
  $ ./hello
  Goodbye world

Run an additional rebuild, which should now do no work
  $ $DODO --show

Add a file to the directory, which should trigger a rebuild
TODO: Once command skipping is implemented, this should just rerun Dodofile
  $ touch foo

Run a rebuild
  $ $DODO --show
  Dodofile
  gcc -o hello hello.c
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Clean up
  $ rm -rf .dodo hello Dodofile foo
  $ cp file_versions/hello-original.c hello.c
