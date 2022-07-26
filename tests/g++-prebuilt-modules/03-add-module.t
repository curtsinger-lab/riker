The modified source file uses <string>. Pre-compile that and update the build.

Move to test directory
  $ cd $TESTDIR

Is g++-11 installed? If not, skip the test.
  $ which g++-11 > /dev/null || exit 80

This test depends on the prior test. Run the program to make sure it's present.
  $ ./program
  HELLO MODULES.

Prebuild the string module
  $ g++-11 --std=c++20 -fmodules-ts -c -x c++-system-header string

Run a rebuild
  $ rkr --show-full
  *cc1plus * (glob)

Run the program again
  $ ./program
  HELLO MODULES.

Run a rebuild
  $ rkr --show-full

Run the program again
  $ ./program
  HELLO MODULES.

Modify main.cc to use an import for string instead of #include
  $ cp files/main.cc.v3 main.cc

Update the build
  $ rkr --show-full
  *cc1plus * (glob)

Run the program again
  $ ./program
  HELLO MODULES.

And run a rebuild
  $ rkr --show-full

Leave the build in place for the next test.
