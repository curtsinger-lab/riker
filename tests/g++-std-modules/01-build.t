Build with modules and make sure a rebuild does no work.

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

Run a rebuild
  $ rkr --show

Run the program again
  $ ./program
  Hello modules.

Clean up
  $ rm -rf .rkr program mapper.txt modules
