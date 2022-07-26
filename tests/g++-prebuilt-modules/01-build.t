Pre-build a standard library module and then build a program using that module.

Move to test directory
  $ cd $TESTDIR

Is g++-11 installed? If not, skip the test.
  $ which g++-11 > /dev/null || exit 80

Prepare for a clean build
  $ rm -rf .rkr program gcm.cache
  $ cp files/main.cc.v1 main.cc

Prebuild the iostream module
  $ g++-11 --std=c++20 -fmodules-ts -c -x c++-system-header iostream

Run the build
  $ rkr --show-full
  rkr-launch
  Rikerfile
  g++-11 * -o program main.cc (glob)
  *cc1plus * (glob)
  as * (glob)
  *collect2 * (glob)
  *ld * (glob)

Run the program
  $ ./program
  Hello modules.

Run a rebuild
  $ rkr --show

Run the program again
  $ ./program
  Hello modules.

Leave the build state in place for the next test.
