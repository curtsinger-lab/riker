Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr output
  $ echo Hello > input

Run the first build
  $ $RKR --show
  rkr-launch
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
  $ $RKR --show
  cat ../input

Check the output
  $ cat output
  Goodbye

Run an additional rebuild
  $ $RKR --show

Check the output again
  $ cat output
  Goodbye

Clean up
  $ rm -rf .rkr working_dir output
  $ echo Hello > input
