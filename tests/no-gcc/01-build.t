Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo hello hello.i hello.o hello.s

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  cpp hello.c hello.i
  [^ ]*cc1 .* (re)
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Run the hello executable
  $ ./hello
  Hello world!
