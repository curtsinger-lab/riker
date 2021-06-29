Test using environment variables that change during a build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr output
  $ echo "hello" > input
  $ gcc -o do_work do_work.c

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  ./do_work

Check the output
  $ cat output
  hello
  goodbye

Run a rebuild
  $ $RKR --show

Check the output
  $ cat output
  hello
  goodbye

Clean up
  $ rm -rf .rkr output do_work
