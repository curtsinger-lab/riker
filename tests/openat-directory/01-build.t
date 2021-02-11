Run a build and verify that no rebuild is nessecary

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr
  $ clang openat-directory.c -o openat-directory
  $ mkdir -p mydir

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  ./openat-directory
  Trying openat(-100, "mydir", 655360, 0)
  Got fd = 3

Clean up
  $ rm -rf .rkr openat-directory mydir