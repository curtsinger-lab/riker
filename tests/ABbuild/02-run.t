Move to test directory
  $ cd $TESTDIR

Run the build
  $ ../../dodo --show
  dodo launch

Verify the output is correct
  $ cat myfile
  hello world

Run the build again, doing nothing this time
  $ ../../dodo --show
