Run the renaming procedure in test.c on file1 and file2, which are hard links to the same file.
Then recreate file2 as a hard link to file1 and run another build.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo file1 file2 test

Build the test program
  $ clang -o test test.c

Set up the file1 and file2 links
  $ touch file1
  $ ln file1 file2

Run the first build
  $ $DODO --show
  dodo-launch
  sh Rikerfile
  ./test
  rename("file2", "file2") failed

Recreate file2 as a hard link to file1
  $ ln file1 file2

Run another build
  $ $DODO --show

Clean up
  $ rm -rf .dodo file1 file2 test
