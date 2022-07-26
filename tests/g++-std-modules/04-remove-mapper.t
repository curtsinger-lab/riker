Build with modules, remove the module map, and then make sure the map is restored on rebuild.

Move to test directory
  $ cd $TESTDIR

Is g++-11 installed? If not, skip the test.
  $ which g++-11 > /dev/null || exit 80

This test depends on the state from the previous build. Make sure it's in place.
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
