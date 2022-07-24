Build with modules, edit a source file, and make sure only that file is compiled before relinking.

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

Clean up
  $ rm -rf .rkr program mapper.txt modules
  $ cp files/main.cc.original main.cc
