Run an initial build

Move to test directory
  $ cd $TESTDIR

Setup
  $ rm -rf .dodo foo Dodofile
  $ clang++ -o Dodofile o_creat.cc
  $ echo "first" > input

Run dodo
  $ $DODO --show
  dodo-launch
  Dodofile
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
  Dodofile
  Error: File exists

Clean up
  $ rm -rf .dodo foo Dodofile
  $ echo "first" > input
