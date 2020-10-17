Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo output
  $ cp input1 input

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
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
  $ $DODO --show
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
  $ $DODO --show

Make sure the output is unchanged
  $ cat output
  a
  b
  d
  e

Clean up
  $ rm -rf .dodo output
  $ cp input1 input
