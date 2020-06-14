Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo hello

Copy in the basic Dodofile and make sure it's executable
  $ cp incremental-Dodofile Dodofile
  $ chmod u+x Dodofile

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  gcc -c -o hello.o hello.c
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  gcc -o hello hello.o
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Run the hello executable
  $ ./hello
  Hello world
