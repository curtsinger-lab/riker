This test contains a program that conditionally acts on the presence
or absence of a file.  If the file is not present, it creates it. On
rebuild, Riker should replay whatever action it did in the last build.

In this test, the final file should contain 'hello'.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr conditional-maker a_file
  $ clang conditional-maker.c -o conditional-maker

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  ./conditional-maker
  'a_file' does not exist.

Check the contents of a_file
  $ cat a_file
  hello

Run the second build
  $ $RKR --show

Check the contents of the file
  $ cat a_file
  hello

Clean up
  $ rm -rf .rkr conditional-maker a_file
