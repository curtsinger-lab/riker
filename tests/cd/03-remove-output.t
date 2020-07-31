Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo output
  $ echo Hello > input
  $ touch output

Run the first build
  $ $DODO --show
  dodo-launch
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
  mv working_dir/output .
  rmdir working_dir

Check the output
  $ cat output
  Hello

Run an additional rebuild, which detects the change in status of output
  $ $DODO --show
  cat ../input
  mv working_dir/output .
  rmdir working_dir

Check the output again
  $ cat output
  Hello

Run a final rebuild, which should do nothing
  $ $DODO --show

Check the output one last time
  $ cat output
  Hello

Clean up
  $ rm -rf .dodo working_dir output
