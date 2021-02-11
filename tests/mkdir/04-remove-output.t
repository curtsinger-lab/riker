Run an initial build, remove the output file, then rebuild

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run. Create an empty output file for now, so rebuilding works
  $ rm -rf .rkr output
  $ echo "Hello" > input

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  mkdir foo
  cat input
  mv foo/f output
  rmdir foo

Check the output
  $ cat output
  Hello

Remove the output file
  $ rm output

Run a rebuild
  $ $RKR --show

Check the output
  $ cat output
  Hello

Run a final rebuild. This should do nothing
  $ $RKR --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .rkr foo
  $ rm output
