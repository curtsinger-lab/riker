Run a build, then rebuild after removing the final output

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo myfile
  $ echo -n "hello" > inputA
  $ echo " world" > inputB

Run the first build
  $ $DODO --show
  dodo-launch
  Rikerfile
  ./A
  cat inputA
  ./B
  cat inputB

Check the output
  $ cat myfile
  hello world

Run a rebuild
  $ $DODO --show

Check the output
  $ cat myfile
  hello world

Remove the output file
  $ rm myfile

Run a rebuild, which should do nothing except restore the file from the cache
  $ $DODO --show

The output file should be back (from the cache)
  $ cat myfile
  hello world

Clean up
  $ rm -rf .dodo myfile
