This checks to see whether we correctly return ELOOP for
symlinks when they are opened O_NOFOLLOW.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr symlink-nofollow a_symlink
  $ clang++ symlink-nofollow.cpp -o symlink-nofollow

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  ./symlink-nofollow
  fd = -1
  Error: Too many levels of symbolic links
  cat a_symlink
  Heya

Clean up
  $ rm -rf .rkr symlink-nofollow a_symlink
