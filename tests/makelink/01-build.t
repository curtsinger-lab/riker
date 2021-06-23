Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f output A B

Set up the inptu file
  $ echo "Hello" > input

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  ln -s input A
  ln -s output B
  cat A
  unlink A
  unlink B

Make sure the output is in place
  $ cat output
  Hello

Run a rebuild, which should do nothing
  $ rkr --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .rkr
  $ rm -f output A B
