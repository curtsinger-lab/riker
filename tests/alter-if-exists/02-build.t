This test contains a program that conditionally acts on the presence
or absence of a file.  If the file is not present, it creates it. On
rebuild, riker should replay whatever action it did in the last build.

In this test, the final file should contain 'hello\nworld'.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr conditional-maker a_file
  $ clang conditional-maker.c -o conditional-maker
  $ echo "hello" > a_file

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  ./conditional-maker
  'a_file' exists.

Check the contents of a_file
  $ cat a_file
  hello
  world

Run the second build
  $ rkr --show

Check the contents of the file
  $ cat a_file
  hello
  world

Clean up
  $ rm -rf .rkr conditional-maker a_file

