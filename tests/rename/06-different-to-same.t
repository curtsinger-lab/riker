Run the renaming procedure in test.c on file1 and file2, which are distinct files.
Then recreate file1 and file2, but this time as hard links to the same inode

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo file1 file2 test

Build the test program
  $ clang -o test test.c

Create file1 and file2
  $ touch file1 file2

Run the first build
  $ $DODO --show
  dodo-launch Dodofile
  sh Dodofile
  ./test
  rename("file1", "file1") failed
  rename("file2", "file2") failed

Recreate file1 and file2, but this time as links to the same inode
  $ touch file1
  $ ln file1 file2

Run another build
  $ $DODO --show
  ./test
  rename("file2", "file2") failed

Clean up
  $ rm -rf .dodo file1 file2 test
