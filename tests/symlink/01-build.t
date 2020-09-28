Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo
  $ rm -f dir1/output dir2/output

Make sure link is a symlink to dir1
  $ rm -f link
  $ ln -s dir1 link

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  cat input1
  cat input2
  cat input3

Check the state of dir1/output
  $ cat dir1/output
  Written to link/output
  Appended to dir1/output

Run a rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat dir1/output
  Written to link/output
  Appended to dir1/output

Clean up
  $ rm -rf .dodo dir1/output dir2/output
