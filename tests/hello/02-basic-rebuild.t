This test checks the rebuild behavior of the build from the previous step

Move to test directory
  $ cd $TESTDIR

With no files changed, the rebuild should not do anything
  $ $DODO --show

Run the hello executable
  $ ./hello
  Hello world

TODO: Test rebuilds with modifications to comments and meaningful source code changes

Clean up
  $ rm -rf .dodo hello Dodofile

SKIP! This test does not work yet
  $ exit 80
