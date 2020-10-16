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

Now change an input file
  $ echo goodbye > hello.txt

Run a rebuild
  $ $DODO --show
  cat hello.txt world.txt

Check the output
  $ cat output
  goodbye
  world

Run a rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output
  goodbye
  world

Clean up
  $ rm -rf .dodo output *.txt
