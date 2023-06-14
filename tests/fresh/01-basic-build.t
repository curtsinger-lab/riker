This test runs a simple gcc build and verifies that fresh flag will do a full build.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr hello

Make sure the Rikerfile is executable
  $ chmod u+x Rikerfile

Run the build
  $ rkr --show --no-wrapper
  rkr-launch
  Rikerfile
  gcc -o hello hello.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Run the build with the fresh flag
  $ rkr --show --no-wrapper --fresh
  rkr-launch
  Rikerfile
  gcc -o hello hello.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)
  
Rerun the build with the fresh flag
  $ rkr --show --no-wrapper --fresh
  rkr-launch
  Rikerfile
  gcc -o hello hello.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)
  
Run the hello executable
  $ ./hello
  Hello world
  
Rerun the build without the fresh flag
  $ rkr --show --no-wrapper

Make sure the hello executable still works
  $ ./hello
  Hello world

Clean up
  $ rm -rf .rkr hello
