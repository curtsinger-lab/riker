Move to test directory
  $ cd $TESTDIR

Move in the new version of inputB
  $ cp file_versions/inputB_new inputB

Update the build
  $ ../../dodo --show
  cat inputB

Verify the output is correct
  $ cat myfile
  goodbye frodo

Run the build again, doing nothing this time
  $ ../../dodo --show

SKIP! This test does not work.
  $ exit 80