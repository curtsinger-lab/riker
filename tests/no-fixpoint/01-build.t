Run a build and verify that no rebuild is nessecary

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo *.num *.err

Run the first build
  $ $DODO --show
  dodo-launch Dodofile
  Dodofile
  ls -1
  wc -l
  touch 2.num

Run the second build-- since nothing WAS changed, nothing SHOULD change
  $ $DODO --show

Clean up
  $ rm -rf .dodo *.num

SKIP! This test doesn't work yet. We need post-build state checking for this to pass.
  $ exit 80
