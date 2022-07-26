Remove modules, one used in the build and one not used.

Move to test directory
  $ cd $TESTDIR

Is g++-11 installed? If not, skip the test.
  $ which g++-11 > /dev/null || exit 80

This test depends on the state from the previous build. Make sure it's in place.
  $ ./program
  Hello modules.

Remove the string module
  $ rm gcm.cache/usr/include/c++/11/string.gcm

Run a rebuild. This shouldn't do anything because the string module was unused.
  $ rkr --show-full

Make sure the program still works
  $ ./program
  Hello modules.

Now remove the iostream module.
  $ rm gcm.cache/usr/include/c++/11/iostream.gcm

Run a rebuild. This will do actual work becuase the module was used.
  $ rkr --show-full
  *cc1plus * (glob)
  as * (glob)
  *ld * (glob)

Run the program
  $ ./program
  Hello modules.

Run a rebuild.
  $ rkr --show-full

Make sure the program still works
  $ ./program
  Hello modules.

Clean up
  $ rm -rf .rkr program gcm.cache
