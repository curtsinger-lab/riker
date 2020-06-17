Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo
  $ rm -f output

Make sure link is a symlink to "HELLO"
  $ rm -f link
  $ ln -s HELLO link

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  readlink link

Check the output
  $ cat output
  HELLO

Run a rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output
  HELLO

Clean up
  $ rm -rf .dodo output
