Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr hello

Copy in the original version of hello.c
  $ cp file_versions/hello-original.c hello.c

Run the build
  $ rkr --show
  rkr-launch
  make --always-make --quiet
  gcc -o hello hello.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Run the hello executable
  $ ./hello
  Hello world

Edit the hello.c file
  $ cp file_versions/hello-modified.c hello.c

Run a rebuild
  $ rkr --show
  cc1 * (glob)
  as * (glob)
  ld * (glob)

Make sure the build worked
  $ ./hello
  Goodbye world

Run another rebuild, which should do nothing
  $ rkr --show

Make sure the output is still there
  $ ./hello
  Goodbye world

Clean up
  $ cp file_versions/hello-original.c hello.c
  $ rm -rf .rkr hello

