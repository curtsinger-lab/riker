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
  Dodofile
  ls -1
  wc -l
  touch 2.num

Clean up
  $ rm -rf .dodo *.num
