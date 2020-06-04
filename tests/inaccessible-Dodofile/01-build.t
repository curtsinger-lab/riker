Move to test directory
  $ cd $TESTDIR

Prepare for a clean build
  $ rm -rf .dodo hello

Make sure the Dodofile is not readable (required for this test)
  $ chmod a-r Dodofile

Build with dodo. This should fail
  $ $DODO
  Unable to access Dodofile.
    This file must be directly executable or runnable with /bin/sh.

Grant permissions for Dodofile again so it can be committed.
  $ chmod a+r Dodofile
