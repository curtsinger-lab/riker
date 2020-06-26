Run a build, remove the output, and then verify it can be restored

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo f

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  touch f

Check the output
  $ cat f

Remove the output
  $ rm f

Run a rebuild
  $ $DODO --show

Check the output
  $ cat f

Clean up
  $ rm -rf .dodo f
