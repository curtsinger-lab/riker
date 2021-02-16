Build a program, then edit comments in the source code to ensure the build stops after cc1

Move to test directory
  $ cd $TESTDIR

Prepare for a full build
  $ rm -rf .rkr hello
  $ cp versions/hello-original.c hello.c

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  gcc -o hello hello.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./hello
  Hello world!

Run a rebuild, which should do nothing
  $ $RKR --show

Check the output
  $ ./hello
  Hello world!

Edit the file, modifying only a comment
  $ cp versions/hello-comment1.c hello.c

Run a rebuild, which should run cc1 and then stop
  $ $RKR --show
  cc1 * (glob)

Check the output
  $ ./hello
  Hello world!

Run a rebuild, which should do nothing
  $ $RKR --show

Check the output
  $ ./hello
  Hello world!

Edit the comment a second time
  $ cp versions/hello-comment2.c hello.c

Run a rebuild, which should run cc1 and then stop
  $ $RKR --show
  cc1 * (glob)

Check the output
  $ ./hello
  Hello world!

Run a rebuild, which should do nothing
  $ $RKR --show

Check the output
  $ ./hello
  Hello world!

Clean up
  $ rm -rf .rkr hello
  $ cp versions/hello-original.c hello.c
