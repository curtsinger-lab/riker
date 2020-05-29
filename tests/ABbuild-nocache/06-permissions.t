Move to test directory
  $ cd $TESTDIR

Mark the buildfile as non-executable (so it runs with /bin/sh)
  $ chmod a-x Dodofile

Run the build
  $ ../../dodo --show --no-caching
  dodo-launch
  /bin/sh Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ cat myfile
  goodbye frodo

Run the build again, doing nothing this time
  $ ../../dodo --show --no-caching

Change the buildfile back to executable
  $ chmod u+x Dodofile

Run the build
  $ ../../dodo --show --no-caching
  dodo-launch
  Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ cat myfile
  goodbye frodo

Run the build again, doing nothing this time
  $ ../../dodo --show --no-caching
