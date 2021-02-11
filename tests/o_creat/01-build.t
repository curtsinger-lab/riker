Run an initial build

Move to test directory
  $ cd $TESTDIR

Setup
  $ rm -rf .dodo foo Rikerfile
  $ clang++ -o Rikerfile o_creat.cc
  $ echo "first" > input

Run dodo
  $ $DODO --show
  dodo-launch
  Rikerfile
  File created with fd = 4

Check the output
  $ cat foo
  Hello world!

Run a rebuild, which should do nothing
  $ $DODO --show

Edit the input file to trigger a rebuild
  $ echo "second" > input

Run a rebuild
  $ $DODO --show
  Rikerfile
  Error: File exists

Clean up
  $ rm -rf .dodo foo Rikerfile
  $ echo "first" > input
