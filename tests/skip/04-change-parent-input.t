Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr parent_output child_output
  $ echo "Hello parent" > parent_input
  $ echo "Hello child" > child_input
  $ make clean all > /dev/null

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  ./parent
  ./child

Check the output
  $ cat parent_output
  Hello parent
  $ cat child_output
  Hello child

Change the parent's input
  $ echo "Goodbye parent" > parent_input

Run a rebuild, which should run only the parent command
  $ rkr --show
  ./parent

Check the output
  $ cat parent_output
  Goodbye parent
  $ cat child_output
  Hello child

Run a rebuild again
  $ rkr --show

Check the output
  $ cat parent_output
  Goodbye parent
  $ cat child_output
  Hello child

Clean up
  $ rm -rf .rkr parent_output child_output
  $ echo "Hello parent" > parent_input
