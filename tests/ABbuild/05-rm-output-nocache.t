Run a build, then rebuild after removing the final output

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr myfile
  $ echo -n "hello" > inputA
  $ echo " world" > inputB

Run the first build
  $ rkr --show --no-caching
  rkr-launch
  Rikerfile
  ./A
  cat inputA
  ./B
  cat inputB

Check the output
  $ cat myfile
  hello world

Run a rebuild
  $ rkr --show --no-caching

Check the output
  $ cat myfile
  hello world

Remove the output file
  $ rm myfile

Run a rebuild, which should do nothing except restore the file from the cache
  $ rkr --show --no-caching
  cat inputA
  cat inputB

The output file should be back (from the cache)
  $ cat myfile
  hello world

Clean up
  $ rm -rf .rkr myfile
