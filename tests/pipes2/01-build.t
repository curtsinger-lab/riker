Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo output
  $ cp input1 input

Run the build
  $ $DODO --show
  dodo-launch
  Rikerfile
  ((cat input)|(sort)|(uniq)) (re)
  ((cat input)|(sort)|(uniq)) (re)
  ((cat input)|(sort)|(uniq)) (re)

Check the output
  $ cat output
  a
  b
  c

Run a rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output
  a
  b
  c

Clean up
  $ rm -rf .dodo output
