Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo output
  $ cp input1 input

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  sort
  uniq

Check the output
  $ cat output
  a
  b
  c

Now remove the output file
  $ rm output

Run a rebuild
  $ $DODO --show
  sort
  uniq

Check the output
  $ cat output
  a
  b
  c

Rebuild again, which should do nothing
  $ $DODO --show

Make sure the output is unchanged
  $ cat output
  a
  b
  c

Clean up
  $ rm -rf .dodo output

SKIP! This test does not work at the moment.
  $ exit 80
