Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo myfile
  $ echo -n "hello" > inputA
  $ echo " world" > inputB

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Check the output
  $ cat myfile
  hello world

Run a rebuild
  $ $DODO --show

Check the output
  $ cat myfile
  hello world

Clean up
  $ rm -rf .dodo myfile
