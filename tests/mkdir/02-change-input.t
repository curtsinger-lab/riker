Run an initial build, then a rebuild with a changed input file

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run. Create an empty output file for now, so rebuilding works
  $ rm -rf .dodo output
  $ echo "Hello" > input

Run the first build
  $ $DODO --show
  dodo-launch
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
  $ $DODO --show
  cat input

Check the output
  $ cat output
  Goodbye

Run an additional rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output
  Goodbye

Clean up
  $ rm -rf .dodo foo
  $ rm output
  $ echo Hello > input
