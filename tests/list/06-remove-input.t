Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo output *.txt
  $ echo hello > hello.txt
  $ echo world > world.txt
  $ touch output

Run the first build
  $ $DODO --show
  dodo-launch Dodofile
  Dodofile
  cat hello.txt world.txt

Check the output
  $ cat output
  hello
  world

Now remove an input file
  $ rm world.txt

Run a rebuild, which should see the changed directory
  $ $DODO --show
  Dodofile
  cat hello.txt

Check the output
  $ cat output
  hello

Run a final rebuild, which should do nothing
  $ $DODO --show

And check the final output
  $ cat output
  hello

Clean up
  $ rm -rf .dodo output *.txt
