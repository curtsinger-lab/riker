This test builds on the first test in this directory. Like the change-link-valid test, the symlink
is updated between runs. Unlike that test, it is updated to a different invalid path. As a result, 
the first access has to rerun, but because the result of the second access (trying to resolve the 
symlink) is unchanged, the second command does not rerun.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo
  $ rm -f output1 output2

Make sure link is a symlink to "HELLO"
  $ rm -f link
  $ ln -s HELLO link

Run the build
  $ $DODO --show
  dodo-launch Dodofile
  Dodofile
  readlink link
  cat link
  cat: link: No such file or directory

Check the output
  $ cat output1
  HELLO
  $ cat output2

Change the link destination
  $ unlink link
  $ ln -s ADIOS link

Rerun the build
  $ $DODO --show
  readlink link

Check the output
  $ cat output1
  ADIOS
  $ cat output2

Rebuild again, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output1
  ADIOS
  $ cat output2

Restore the link state
  $ unlink link
  $ ln -s HELLO link

Clean up
  $ rm -rf .dodo
  $ rm -f output1 output2
