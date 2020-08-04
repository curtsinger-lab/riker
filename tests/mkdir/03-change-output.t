Run an initial build, change the output file, then rebuild

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run. Create an empty output file for now, so rebuilding works
  $ rm -rf .dodo output
  $ echo "Hello" > input
  $ touch output

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  mkdir foo
  cat input
  mv foo/f output
  rmdir foo

Check the output
  $ cat output
  Hello

Change the output file. Sleep first to make sure the output file's mtime changes.
  $ sleep 0.1
  $ echo "OUTPUT" > output

Run a rebuild
  $ $DODO --show
  cat input

Check the output
  $ cat output
  Hello

Run an additional rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .dodo foo
  $ rm output
