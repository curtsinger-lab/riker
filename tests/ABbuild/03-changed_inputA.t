Move to test directory
  $ cd $TESTDIR

Move in the new version of inputA
  $ cp file_versions/inputA_new inputA

Update the build
  $ ../../dodo --show
  /bin/sh ./A

Verify the output is correct
  $ cat myfile
  goodbye world

Run the build again, doing nothing this time
  $ ../../dodo --show
