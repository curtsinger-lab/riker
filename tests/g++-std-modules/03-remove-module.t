Build with modules, remove one of the precompiled modules, and then make sure the precompiled module is restored when updating the build.

Move to test directory
  $ cd $TESTDIR

Is g++-11 installed? If not, skip the test.
  $ which g++-11 > /dev/null || exit 80

This test depends on the prior test. Run the program to make sure it's present.
  $ ./program
  HELLO MODULES.

Remove the iostream module and edit main.cc
  $ rm modules/iostream.gcm
  $ cp files/main.cc.original main.cc

Run a rebuild
  $ rkr --show-full
  *cc1plus * (glob)
  as * (glob)
  *ld * (glob)

Make sure iostream.gcm was restored
  $ file modules/iostream.gcm
  modules/iostream.gcm: ELF 32-bit LSB no file type, no machine, version 1 (SYSV)

Run the program again
  $ ./program
  Hello modules.

Run a rebuild
  $ rkr --show-full

Run the program again
  $ ./program
  Hello modules.

Leave the build in place for the next test.
