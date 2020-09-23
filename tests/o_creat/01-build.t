Run an initial build

Move to test directory
  $ cd $TESTDIR

Setup
  $ rm -rf .dodo foo Dodofile
  $ clang++ -o Dodofile o_creat.cc

Run dodo
  $ $DODO --show
  dodo-launch
  Dodofile
  File created with fd = 3

Check the output
  $ cat foo
  Hello world!

Run a rebuild
  $ $DODO --show
  Dodofile
  Error: File exists

Clean up
  $ rm -rf .dodo foo Dodofile
