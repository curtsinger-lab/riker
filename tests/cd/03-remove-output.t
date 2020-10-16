Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo output
  $ echo Hello > input

Run the first build
  $ $DODO --show
  dodo-launch Dodofile
  Dodofile
  mkdir working_dir
  cat ../input
  mv working_dir/output .
  rmdir working_dir

Check the output
  $ cat output
  Hello

Remove the output file
  $ rm output

Run a rebuild
  $ $DODO --show
  cat ../input

Check the output
  $ cat output
  Hello

Run an additional rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .dodo working_dir output
