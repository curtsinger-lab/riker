Run a build with cmake under Riker tracing, then an incremental build after editing the source

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

Modify test.c and rebuild
  $ cp versions/test-modified.c test.c
  $ rkr --show
  cmake ** (glob)
  Scanning dependencies of target test
  cc1 ** (glob)
  as ** (glob)
  ld ** (glob)

Check the output again
  $ build/test
  Goodbye CMake!

Run a rebuild
  $ rkr --show

Check the output one final time

Clean up
  $ rm -rf .rkr build
  $ cp versions/test-original.c test.c
