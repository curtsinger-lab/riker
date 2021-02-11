Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo output
  $ echo Hello > input

Run the first build
  $ $DODO --show
  dodo-launch
  Rikerfile
  mkdir working_dir
  cat ../input
  mv working_dir/output .
  rmdir working_dir

Check the output
  $ cat output
  Hello

Change the input file
  $ echo Goodbye > input

Run a rebuild
  $ $DODO --show
  cat ../input

Check the output
  $ cat output
  Goodbye

Run an additional rebuild
  $ $DODO --show

Check the output again
  $ cat output
  Goodbye

Clean up
  $ rm -rf .dodo working_dir output
  $ echo Hello > input
