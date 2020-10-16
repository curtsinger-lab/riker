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

Now create a new input file
  $ echo goodbye > goodbye.txt

Run a rebuild, which should see the changed directory
  $ $DODO --show
  Dodofile
  cat goodbye.txt hello.txt world.txt

Check the output
  $ cat output
  goodbye
  hello
  world

Run a final rebuild, which should do nothing
  $ $DODO --show

And check the final output
  $ cat output
  goodbye
  hello
  world

Clean up
  $ rm -rf .dodo output *.txt
