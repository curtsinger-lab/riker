Run a build and verify that no rebuild is nessecary

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo f

Run the first build
  $ $DODO --show
  dodo-launch
  Rikerfile
  touch f

Check the output
  $ cat f

Run a rebuild
  $ $DODO --show

Check the output
  $ cat f 

Clean up
  $ rm -rf .dodo f
