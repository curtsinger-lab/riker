Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr core abort
  $ clang -g -o abort abort.c

Run the abort program; dodo should eventually print 'core exists' and not die beforehand
  $ $RKR --show
  rkr-launch
  Rikerfile
  ./abort
  Aborted (core dumped)
  core exists

Clean up
  $ rm -rf .rkr core abort

SKIP! This test doesn't work yet
  $ exit 80
