Edit the main source file and update the build again.

Move to test directory
  $ cd $TESTDIR

Is g++-11 installed? If not, skip the test.
  $ which g++-11 > /dev/null || exit 80

This test depends on the prior test. Run the program to make sure it's present.
  $ ./program
  HELLO MODULES.

Undo the edit to main.cc
  $ cp files/main.cc.v1 main.cc

Run a rebuild
  $ rkr --show-full
  *cc1plus * (glob)
  as * (glob)
  *ld * (glob)

Run the program again
  $ ./program
  Hello modules.

Run a rebuild
  $ rkr --show-full

Run the program again
  $ ./program
  Hello modules.

Leave the build in place for the next test.
