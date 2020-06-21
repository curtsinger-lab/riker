Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo hello

Touch the hello file, since make will stat it
  $ touch hello

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  make --always-make --quiet -j1
  gcc -o hello hello.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Run the hello executable
  $ ./hello
  Hello world

Edit the hello.c file
  $ cp hello.c saved-hello.c
  $ cp goodbye.c hello.c

Run a rebuild
  $ $DODO --show
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Make sure the build worked
  $ ./hello
  Goodbye world

Run another rebuild, which should do nothing
  $ $DODO --show

Make sure the output is still there
  $ ./hello
  Goodbye world

Clean up
  $ mv saved-hello.c hello.c
  $ rm -rf .dodo hello

SKIP! This test does not work yet. The post-edit build reruns make.
  $ exit 80
