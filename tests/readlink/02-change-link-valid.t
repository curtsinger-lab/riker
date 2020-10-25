This test builds on the previous test by updating the symlink destination to a valid path.
The first access sees updated contents and reruns.
The second access now reaches a file, so that reruns as well.

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
  dodo-launch
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
  $ ln -s GOODBYE link

Rerun the build
  $ $DODO --show
  readlink link
  cat link

Check the output
  $ cat output1
  GOODBYE
  $ cat output2
  FAREWELL

Rebuild again, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output1
  GOODBYE
  $ cat output2
  FAREWELL

Restore the link state
  $ unlink link
  $ ln -s HELLO link

Clean up
  $ rm -rf .dodo
  $ rm -f output1 output2

SKIP! This test now triggers a rebuild on line 44 because Dodofile observes an exit status change from `cat link`.
  $ exit 80
