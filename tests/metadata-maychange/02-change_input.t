Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo a b
  $ echo "hello" > a_input
  $ echo "world" > b_input
  $ chmod 0644 b_input

Compile a and b
  $ clang -o a a.c
  $ clang -o b b.c

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  ./a
  ./b

Change the contents of a_input
  $ echo "goodbye" > a_input

Check the rebuild plan. We know a must rerun, but b may have to run because a changes its input metadata
  $ $DODO check
  Commands that must run:
    ./a
  
  Commands that may run:
    ./b

Now run the build. The b command will not run once iterative builds are finished
  $ $DODO --show
  ./a
  ./b

Clean up
  $ rm -rf .dodo a b
  $ echo "hello" > a_input
  $ chmod 0644 b_input
