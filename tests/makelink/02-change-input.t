Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo
  $ rm -f output

Set up the inptu file
  $ echo "Hello" > input

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  ln -s input A
  ln -s output B
  ./copy_data
  cat A
  unlink A
  unlink B

Make sure the output is in place
  $ cat output
  Hello

Now change input
  $ echo "Goodbye" > input

Run a rebuild, which only needs to rerun cat
  $ $DODO --show
  ln -s input A
  ln -s output B
  ./copy_data
  cat A
  unlink A
  unlink B

Check the output
  $ cat output
  Goodbye

Clean up
  $ rm -rf .dodo
  $ rm -f output
  $ echo "Hello" > input
