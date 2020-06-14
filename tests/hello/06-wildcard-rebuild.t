This test checks the rebuild behavior of the build from the previous step

Move to test directory
  $ cd $TESTDIR

Dodofile used a wildcard and will need to rerun (dir contents changed) but that's it
  $ $DODO --show
  Dodofile

Run the hello executable
  $ ./hello
  Hello world

TODO: Test rebuilds with modifications to comments and meaningful source code changes

Clean up
  $ rm -rf .dodo hello Dodofile

SKIP! This test does not work yet
  $ exit 80
