Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo output
  $ cp input1 input

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  ((sort)|(uniq)) (re)
  ((sort)|(uniq)) (re)

Check the output
  $ cat output
  a
  b
  c

SKIP! This test deadlocks because pipe setup on incremental builds is not quite right.
  $ exit 80

Now remove the output file
  $ rm output

Run a rebuild
  $ $DODO --show
  ((sort)|(uniq)) (re)
  ((sort)|(uniq)) (re)

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
