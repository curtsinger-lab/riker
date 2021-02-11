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

Now remove an input file
  $ rm world.txt

Run a rebuild, which should see the changed directory
  $ $RKR --show
  Rikerfile
  cat hello.txt

Check the output
  $ cat output
  hello

Run a final rebuild, which should do nothing
  $ $RKR --show

And check the final output
  $ cat output
  hello

Clean up
  $ rm -rf .rkr output *.txt
