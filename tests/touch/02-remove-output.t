Run a build, remove the output, and then verify it can be restored

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr f

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  touch f

Check the output
  $ cat f

Remove the output
  $ rm f

Run a rebuild
  $ rkr --show

Check the output
  $ cat f

Clean up
  $ rm -rf .rkr f
