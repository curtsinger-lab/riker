Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr myfile
  $ echo -n "hello" > inputA
  $ echo " world" > inputB

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  ./A
  cat inputA
  ./B
  cat inputB

Check the output
  $ cat myfile
  hello world

Run a rebuild
  $ rkr --show

Check the output
  $ cat myfile
  hello world

Clean up
  $ rm -rf .rkr myfile
