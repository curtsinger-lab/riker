Run the renaming procedure in test.c on file1 and file2, which are distinct files.
Then recreate file2 as a distinct file and rerun the build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr file1 file2 test

Build the test program
  $ clang -o test test.c

Create file1 and file2
  $ touch file1 file2

Run the first build
  $ rkr --show
  rkr-launch
  sh Rikerfile
  ./test
  rename("file1", "file1") failed
  rename("file2", "file2") failed

Recreate file1 and file2
  $ touch file1 file2

Run another build
  $ rkr --show

Clean up
  $ rm -rf .rkr file1 file2 test
