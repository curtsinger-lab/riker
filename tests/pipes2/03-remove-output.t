Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr output
  $ cp input1 input

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  ((cat input)|(sort)|(uniq)) (re)
  ((cat input)|(sort)|(uniq)) (re)
  ((cat input)|(sort)|(uniq)) (re)

Check the output
  $ cat output
  a
  b
  c

Now remove the output file
  $ rm output

Run a rebuild
  $ rkr --show

Check the output
  $ cat output
  a
  b
  c

Rebuild again, which should do nothing
  $ rkr --show

Make sure the output is unchanged
  $ cat output
  a
  b
  c

Clean up
  $ rm -rf .rkr output
