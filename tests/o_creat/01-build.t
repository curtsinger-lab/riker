Run an initial build

Move to test directory
  $ cd $TESTDIR

Setup
  $ rm -rf .rkr foo Rikerfile
  $ clang++ -o Rikerfile o_creat.cc
  $ echo "first" > input

Run riker
  $ rkr --show
  rkr-launch
  Rikerfile
  File created with fd = 4

Check the output
  $ cat foo
  Hello world!

Run a rebuild, which should do nothing
  $ rkr --show

Edit the input file to trigger a rebuild
  $ echo "second" > input

Run a rebuild
  $ rkr --show
  Rikerfile
  Error: File exists

Clean up
  $ rm -rf .rkr foo Rikerfile
  $ echo "first" > input
