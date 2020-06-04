Move to test directory
  $ cd $TESTDIR

Move in the new version of inputB
  $ cp file_versions/inputB_new inputB

Update the build
  $ $DODO --show --no-caching
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ cat myfile
  goodbye frodo

Run the build again, doing nothing this time
  $ $DODO --show --no-caching
