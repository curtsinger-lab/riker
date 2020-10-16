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

Now remove the output file
  $ rm output

Run a rebuild, which has to regenerate the output file
  $ $DODO --show
  cat hello.txt world.txt

Check the output
  $ cat output
  hello
  world

Run a final rebuild, which should do nothing
  $ $DODO --show

And check the final output
  $ cat output
  hello
  world

Clean up
  $ rm -rf .dodo output *.txt
