Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr output *.txt
  $ echo hello > hello.txt
  $ echo world > world.txt

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  cat hello.txt world.txt

Check the output
  $ cat output
  hello
  world

Now create a new input file
  $ echo goodbye > goodbye.txt

Run a rebuild, which should see the changed directory
  $ rkr --show
  Rikerfile
  cat goodbye.txt hello.txt world.txt

Check the output
  $ cat output
  goodbye
  hello
  world

Run a final rebuild, which should do nothing
  $ rkr --show

And check the final output
  $ cat output
  goodbye
  hello
  world

Clean up
  $ rm -rf .rkr output *.txt
