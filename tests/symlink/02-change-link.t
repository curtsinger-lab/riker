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
  $ rkr --show
  rkr-launch
  Rikerfile
  cat input1
  cat input2
  cat input3

Check the state of dir1/output
  $ cat dir1/output
  Written to link/output
  Appended to dir1/output

Now change the link to refer to dir2
  $ rm link
  $ rm -f dir1/output dir2/output
  $ ln -s dir2 link

Run a rebuild
  $ rkr --show
  cat input3

Check the stat of dir1/output and dir2/output
  $ cat dir1/output
  Written to dir1/output
  Appended to dir1/output

  $ cat dir2/output
  Written to link/output

Put the link back how it was
  $ unlink link
  $ ln -s dir1 link

Clean up
  $ rm -rf .rkr 
  $ rm -rf dir1 dir2