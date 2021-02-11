This test is intended to check whether we correctly start ancestor commands
on rebuild and whether we also uniformly treat the outputs of ancsestor
commands as changed.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr output-*

Run the build
  $ $RKR --show
  rkr-launch
  Rikerfile
  python3 Rikerfile
  python3 build 3
  [^ ]*cat /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*python3 build 2 /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*cat /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*python3 build 1 /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*cat /tmp/cramtests.*/tmp/tmp.* (re)
  [^ ]*python3 build 0 /tmp/cramtests.*/tmp/tmp.* (re)

Rebuild without changing anything, which should do nothing
  $ $RKR --show

Clean up
  $ rm -rf .rkr rm output-*

SKIP! This test is sensitive to the /proc filesystem. Once we properly exclude these paths this test should work again.
  $ exit 80