Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr output *.txt
  $ echo hello > hello.txt
  $ echo world > world.txt

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  cat hello.txt world.txt

Check the output
  $ cat output
  hello
  world

Now remove the output file
  $ rm output

Run a rebuild, which should copy the output file from cache
  $ $RKR --show

Check the output
  $ cat output
  hello
  world

Remove the output again
  $ rm output

Run a rebuild, this time without caching
  $ $RKR --show --no-caching
  cat hello.txt world.txt

Check the output
  $ cat output
  hello
  world

Run a final rebuild, which should do nothing
  $ $RKR --show

And check the final output
  $ cat output
  hello
  world

Clean up
  $ rm -rf .rkr output *.txt
