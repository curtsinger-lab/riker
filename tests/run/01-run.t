This test runs a simple gcc build and verifies that a rebuild does no work.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr hello

Run the build
  $ rkr run --show-full "gcc -o hello hello.c"
  gcc -o hello hello.c gcc -o hello hello.c
  [^ ]*sh -c gcc -o hello hello.c .* (re)
  gcc -o hello hello.c
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Run the hello executable
  $ ./hello
  Hello world

Run a rebuild, which should do nothing.
  $ rkr --show

Make sure the hello executable still works
  $ ./hello
  Hello world

Clean up
  $ rm -rf .rkr hello Rikerfile
