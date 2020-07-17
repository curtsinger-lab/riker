Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo output *.txt
  $ echo hello > hello.txt
  $ echo world > world.txt

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  cat hello.txt world.txt

Check the output
  $ cat output
  hello
  world

Run a rebuild, which should now detect the new output file
  $ $DODO --show
  Dodofile
  cat hello.txt world.txt

Check the output again
  $ cat output
  hello
  world

Run an additional rebuild, which now sees no change
  $ $DODO --show

Check the output
  $ cat output
  hello
  world

Clean up
  $ rm -rf .dodo output *.txt
