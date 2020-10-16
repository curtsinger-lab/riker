This test is intended to check whether we correctly start ancestor commands
on rebuild and whether we also uniformly treat the outputs of ancsestor
commands as changed.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo output-*

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  python3 Dodofile
  python3 build 3
  [^ ]*cat /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*python3 build 2 /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*cat /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*python3 build 1 /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*cat /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*python3 build 0 /tmp/cramtests.*/tmp/tmp.* (re)

Rebuild without changing anything, which should do nothing
  $ $DODO --show

Clean up
  $ rm -rf .dodo rm output-*