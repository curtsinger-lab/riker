Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr output
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

Remove the output file
  $ rm output

Run a rebuild
  $ rkr --show

Check the output
  $ cat output
  Hello

Remove the output file again
  $ rm output

Run a rebuild without caching
  $ rkr --show --no-caching
  cat ../input

Check the output
  $ cat output
  Hello

Run an additional rebuild, which will need to rerun cat ../input
TODO: Falling back to a hash of the committed state breaks because we don't compare hashes at commit time. Once that's working this run should do nothing.
  $ rkr --show
  cat ../input

Check the output again
  $ cat output
  Hello

Run one additional rebuild, which should do nothing
  $ rkr --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .rkr working_dir output
