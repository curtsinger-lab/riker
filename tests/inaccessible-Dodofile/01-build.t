Move to test directory
  $ cd $TESTDIR

Prepare for a clean build
  $ rm -f db.dodo hello

Make sure the Dodofile is not readable (required for this test)
  $ chmod a-r Dodofile

Build with dodo. This should fail
  $ ../../dodo
  Unable to access Dodofile, which is required for the build.
  See http://dodo.build for instructions.
  [1]

Grant permissions for Dodofile again so it can be committed.
  $ chmod a+r Dodofile
