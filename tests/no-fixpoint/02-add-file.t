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
  grep .num
  wc -l
  touch 0.num

Now create an additional file, which changes an input to the build
  $ touch 1.num

Run a rebuild
  $ $DODO --show
  ls -1
  grep .num
  wc -l
  touch 2.num

Run an additional rebuild, which should do nothing
  $ $DODO --show

Clean up
  $ rm -rf .dodo *.num

SKIP! Something is wrong with the pipes in this test.
  $ exit 80
