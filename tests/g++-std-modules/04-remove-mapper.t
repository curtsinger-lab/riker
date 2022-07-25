Build with modules, remove the module map, and then make sure the map is restored on rebuild.

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

Remove the module map
  $ rm mapper.txt

Run a rebuild
  $ rkr --show-full
  Rikerfile
  rm -rf modules mapper.txt

Make sure the map was restored
  $ file mapper.txt
  mapper.txt: ASCII text

Make sure the program still works
  $ ./program
  Hello modules.

Clean up
  $ rm -rf .rkr program mapper.txt modules
