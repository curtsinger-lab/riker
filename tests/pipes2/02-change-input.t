Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr output
  $ cp input1 input

Run the build
  $ $RKR --show
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

Now change the input
  $ cp input2 input

Run a rebuild
  $ $RKR --show
  ((cat input)|(sort)|(uniq)) (re)
  ((cat input)|(sort)|(uniq)) (re)
  ((cat input)|(sort)|(uniq)) (re)

Check the output
  $ cat output
  a
  b
  d
  e

Rebuild again, which should do nothing
  $ $RKR --show

Make sure the output is unchanged
  $ cat output
  a
  b
  d
  e

Clean up
  $ rm -rf .rkr output
  $ cp input1 input
