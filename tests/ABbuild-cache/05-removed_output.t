Move to test directory
  $ cd $TESTDIR

Remove the output myfile
  $ rm myfile

Update the build
  $ $DODO --show

Verify the output is correct
  $ cat myfile
  goodbye frodo

Run the build again, doing nothing this time
  $ $DODO --show

SKIP! This test does not work.
  $ exit 80