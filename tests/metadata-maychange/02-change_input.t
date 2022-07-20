Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr a b
  $ echo "hello" > a_input
  $ echo "world" > b_input
  $ chmod 0644 b_input

Compile a and b
  $ clang -o a a.c
  $ clang -o b b.c

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  ./a
  ./b

Change the contents of a_input
  $ echo "goodbye" > a_input

Now run the build. The b command does not need to run because it depends only on metadata, which ./a does not change
  $ rkr --show
  ./a

Clean up
  $ rm -rf .rkr a b
  $ echo "hello" > a_input
  $ chmod 0644 b_input
