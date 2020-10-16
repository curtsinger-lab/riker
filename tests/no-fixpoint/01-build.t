Run a build and verify that no rebuild is nessecary

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo *.num *.err

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  ls -1
  grep .num
  wc -l
  touch 0.num

Run the second build-- since nothing WAS changed, nothing SHOULD change
  $ $DODO --show

Clean up
  $ rm -rf .dodo *.num
