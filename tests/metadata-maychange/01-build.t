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

Run a rebuild
  $ rkr --show

Clean up
  $ rm -rf .rkr a b
  $ chmod 0644 b_input
