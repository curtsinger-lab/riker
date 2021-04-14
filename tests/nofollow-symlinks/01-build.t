This checks to see whether we correctly return ELOOP for
symlinks when they are opened O_NOFOLLOW.

SKIP! This tests triggers a mismatch between the openat() result and our model.
We need to fix the handling for openat(AT_FDCWD, "link", O_NOFOLLOW | O_CREAT) when "link" is a symlink to a non-existing path.
  $ exit 80

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
