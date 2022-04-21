Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr core abort
  $ clang -g -o abort abort.c

Does running the program actually produce a core file? If not, skip this test
  $ ./abort
  Aborted (core dumped)
  $ test -f core || exit 80
  $ rm -f core

Run the abort program; riker should eventually print 'core exists' and not die beforehand
  $ rkr --show
  rkr-launch
  Rikerfile
  ./abort
  Aborted (core dumped)
  core exists

Clean up
  $ rm -rf .rkr core abort
