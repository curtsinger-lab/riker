Run the renaming procedure in test.c on file1 and file2, which are hard links to the same file

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr file1 file2 test

Build the test program
  $ clang -o test test.c

Set up the file1 and file2 links
  $ touch file1
  $ ln file1 file2

Run the first build
  $ rkr --show
  rkr-launch
  sh Rikerfile
  ./test
  rename("file2", "file2") failed

Clean up
  $ rm -rf .rkr file1 file2 test
