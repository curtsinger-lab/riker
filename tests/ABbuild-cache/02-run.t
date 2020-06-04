Move to test directory
  $ cd $TESTDIR

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ cat myfile
  hello world

Run the build again, doing nothing this time
  $ $DODO --show
