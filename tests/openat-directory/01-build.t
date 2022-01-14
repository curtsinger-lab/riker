Run a build and verify that no rebuild is nessecary

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr
  $ clang openat-directory.c -o openat-directory
  $ mkdir -p mydir

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  ./openat-directory
  Trying openat(-100, "mydir", O_RDONLY|O_CLOEXEC|O_NOFOLLOW, 0)
  Got fd = 3

Clean up
  $ rm -rf .rkr openat-directory mydir