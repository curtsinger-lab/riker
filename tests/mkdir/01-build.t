Run an initial build and a rebuild with no changes

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run. Create an empty output file for now, so rebuilding works
  $ rm -rf .rkr output
  $ echo "Hello" > input

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  mkdir foo
  cat input
  mv foo/f output
  rmdir foo

Check the output
  $ cat output
  Hello

Run a rebuild
  $ rkr --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .rkr foo
  $ rm output
