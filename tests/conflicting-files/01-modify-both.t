This test runs a simple gcc build, modifies files to force a rebuild of multiple source files, and then verifies that the final output is correct.

Passing this test requires caching the .s files in /tmp because gcc uses the same path to hold output from both cc1 invocations.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr hello
  $ cp versions/hello-original.c hello.c
  $ cp versions/goodbye-original.c goodbye.c

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  gcc -o hello hello.c goodbye.c
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Run the hello executable
  $ ./hello
  Hello.
  Goodbye.

Run a rebuild, which should do nothing.
  $ rkr --show

Make sure the hello executable still works
  $ ./hello
  Hello.
  Goodbye.

Now modify both .c files
  $ cp versions/hello-modified.c hello.c
  $ cp versions/goodbye-modified.c goodbye.c

Now run a rebuild, which should rerun both cc1 commands, both as commands, and the linker
  $ rkr --show
  [^ ]*cc1 .* (re)
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*as .* (re)
  [^ ]*ld .* (re)

Make sure the hello executable works
  $ ./hello
  Hello!
  Goodbye!

Clean up
  $ rm -rf .rkr hello
  $ cp versions/hello-original.c hello.c
  $ cp versions/goodbye-original.c goodbye.c
