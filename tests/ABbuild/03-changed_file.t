Move to test directory
  $ cd $TESTDIR

Move in the new version of inputB
  $ cp file_versions/inputB_new inputB

Update the build
  $ ../../dodo

Verify the output is correct
  $ cat myfile
  hello frodo

SKIP! This test does not work.
  $ exit 80
