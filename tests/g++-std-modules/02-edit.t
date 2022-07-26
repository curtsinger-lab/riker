Build with modules, edit a source file, and make sure only that file is compiled before relinking.

Move to test directory
  $ cd $TESTDIR

Is g++-11 installed? If not, skip the test.
  $ which g++-11 > /dev/null || exit 80

This test depends on the completed build state from the prior test.

Run the program to make sure the build is in place.
  $ ./program
  Hello modules.

Move in a modified main.cc
  $ cp files/main.cc.modified main.cc

Run a rebuild
  $ rkr --show-full
  *cc1plus * (glob)
  as * (glob)
  *ld * (glob)

Run the program again
  $ ./program
  HELLO MODULES.

Run a rebuild
  $ rkr --show-full

Run the program again
  $ ./program
  HELLO MODULES.

Leave the build state in place for the next test.
