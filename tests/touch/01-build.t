Run a build and verify that no rebuild is nessecary

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr f

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  touch f

Check the output
  $ cat f

Run a rebuild
  $ $RKR --show

Check the output
  $ cat f 

Clean up
  $ rm -rf .rkr f
