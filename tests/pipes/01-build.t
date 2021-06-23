Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr output
  $ cp input1 input

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  ((sort)|(uniq)) (re)
  ((sort)|(uniq)) (re)

Check the output
  $ cat output
  a
  b
  c

Run a rebuild, which should do nothing
  $ rkr --show

Check the output again
  $ cat output
  a
  b
  c

Clean up
  $ rm -rf .rkr output
