This test accesses a symlink in two ways:
1. Read the destination of a symlink, then write that out to another file
2. Access through a symlink, then write the contents of the reached file to another output
The symlink references a non-existent path, so the first access succeeds and the second fails.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f output1 output2

Make sure link is a symlink to "HELLO"
  $ rm -f link
  $ ln -s HELLO link

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  readlink link
  cat link
  cat: link: No such file or directory

Check the output
  $ cat output1
  HELLO
  $ cat output2

Run a rebuild, which should do nothing
  $ rkr --show

Check the output again
  $ cat output1
  HELLO
  $ cat output2

Clean up
  $ rm -rf .rkr
  $ rm -f output1 output2
