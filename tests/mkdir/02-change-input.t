Run an initial build, then a rebuild with a changed input file

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

Change the input file
  $ echo "Goodbye" > input

Run a rebuild
  $ rkr --show
  cat input

Check the output
  $ cat output
  Goodbye

Run an additional rebuild, which should do nothing
  $ rkr --show

Check the output again
  $ cat output
  Goodbye

Clean up
  $ rm -rf .rkr foo
  $ rm output
  $ echo Hello > input
