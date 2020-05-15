Move to test directory
  $ cd $TESTDIR

Run the build
  $ ../../dodo --show --no-caching
  dodo launch
  Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ cat myfile
  hello world

Run the build again, doing nothing this time
  $ ../../dodo --show --no-caching
