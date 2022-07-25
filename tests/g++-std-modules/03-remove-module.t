Build with modules, remove one of the precompiled modules, and then make sure the precompiled module is restored when updating the build.

Move to test directory
  $ cd $TESTDIR

Is g++-11 installed? If not, skip the test.
  $ which g++-11 > /dev/null || exit 80

Prepare for a clean build
  $ rm -rf .rkr program mapper.txt modules
  $ cp files/main.cc.original main.cc

Run the build
  $ rkr --show-full
  rkr-launch
  Rikerfile
  rm -rf modules mapper.txt
  mkdir -p modules
  g++-11 * -c -x c++-system-header iostream (glob)
  *cc1plus * (glob)
  g++-11 * -c -x c++-system-header string (glob)
  *cc1plus * (glob)
  g++-11 * -o program main.cc (glob)
  *cc1plus * (glob)
  as * (glob)
  *collect2 * (glob)
  *ld * (glob)

Run the program
  $ ./program
  Hello modules.

Remove the iostream module and edit main.cc
  $ rm modules/iostream.gcm
  $ cp files/main.cc.modified main.cc

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
  HELLO MODULES.

Run a rebuild
  $ rkr --show-full

Run the program again
  $ ./program
  HELLO MODULES.

Clean up
  $ rm -rf .rkr program mapper.txt modules
  $ cp files/main.cc.original main.cc
