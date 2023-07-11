This test runs a simple program that accesses a file under /proc/self/fd/

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr prog output

Compile prog
  $ clang -o prog prog.c

Run the build
  $ rkr --show --no-wrapper
  rkr-launch
  Rikerfile
  ./prog

Print the output file
  $ cat output
  X

Run a rebuild, which should do nothing.
  $ rkr --show

Print the output file
  $ cat output
  X

Clean up
  $ rm -rf .rkr prog output
