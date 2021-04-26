Run an initial build and a rebuild with no changes

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run. Create an empty output file for now, so rebuilding works
  $ rm -rf .rkr program z.*

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile

Check the output
  $ cat output
  Hello

Run a rebuild
  $ $RKR --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .rkr foo
  $ rm output
