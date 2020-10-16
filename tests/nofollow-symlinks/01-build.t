This checks to see whether we correctly return ELOOP for
symlinks when they are opened O_NOFOLLOW.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo symlink-nofollow a_symlink
  $ clang++ symlink-nofollow.cpp -o symlink-nofollow

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  ./symlink-nofollow
  fd = -1
  Error: Too many levels of symbolic links
  cat a_symlink
  Heya

Clean up
  $ rm -rf .dodo symlink-nofollow a_symlink