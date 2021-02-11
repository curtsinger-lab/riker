Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f dir1/output dir2/output

Setup
  $ mkdir -p dir1 dir2

Make sure link is a symlink to dir1
  $ rm -f link
  $ ln -s dir1 link

Run the build
  $ $RKR --show
  rkr-launch
  Rikerfile
  cat input1
  cat input2
  cat input3

Check the state of dir1/output
  $ cat dir1/output
  Written to link/output
  Appended to dir1/output

Run a rebuild, which should do nothing
  $ $RKR --show

Check the output again
  $ cat dir1/output
  Written to link/output
  Appended to dir1/output

Clean up
  $ rm -rf .rkr dir1 dir2