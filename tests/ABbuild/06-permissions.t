Move to test directory
  $ cd $TESTDIR

Mark the buildfile as non-executable (so it runs with /bin/sh)
  $ chmod a-x Dodofile

Run the build
  $ ../../dodo --show
  dodo launch

Verify the output is correct
  $ cat myfile
  goodbye frodo

Run the build again, doing nothing this time
  $ ../../dodo --show

Change the buildfile back to executable
  $ chmod u+x Dodofile

Run the build
  $ ../../dodo --show
  dodo launch

Verify the output is correct
  $ cat myfile
  goodbye frodo

Run the build again, doing nothing this time
  $ ../../dodo --show
