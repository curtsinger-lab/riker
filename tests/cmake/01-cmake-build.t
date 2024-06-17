Run a build with cmake under Riker tracing

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr build
  $ cp versions/test-original.c test.c

Run the first build
  $ rkr --no-wrapper > /dev/null

Check the output
  $ build/test
  Hello CMake!

Rebuilding should do nothing
  $ rkr --show

Check the output again
  $ build/test
  Hello CMake!

Clean up
  $ rm -rf .rkr build
