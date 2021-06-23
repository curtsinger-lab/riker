Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr output working_dir
  $ echo Hello > input

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  mkdir working_dir
  cat ../input
  mv working_dir/output .
  rmdir working_dir

Check the output
  $ cat output
  Hello

Run a rebuild
  $ rkr --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .rkr working_dir output
