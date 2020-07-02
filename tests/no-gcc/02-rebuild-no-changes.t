This test checks the rebuild behavior of the build from the previous step

Move to test directory
  $ cd $TESTDIR

With no files changed, the rebuild should not do anything
  $ $DODO --show

Run the hello executable
  $ ./hello
  Hello world!