Move to test directory
  $ cd $TESTDIR

Remove the output myfile
  $ rm myfile

Update the build
  $ $DODO --show --no-caching
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ cat myfile
  goodbye frodo

Run the build again, doing nothing this time
  $ $DODO --show --no-caching
