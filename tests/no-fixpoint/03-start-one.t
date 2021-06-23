Run a build that reads the contents of the current directory and creates a new file.
Start with one matching file in the directory, then verify that a rebuild does no work.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr *.num
  $ touch 0.num

Run the first build (note that commands in (re) below race)
  $ rkr --show
  rkr-launch
  Rikerfile
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  touch 1.num

Run the second build-- since nothing WAS changed, nothing SHOULD change
  $ rkr --show

Clean up
  $ rm -rf .rkr *.num
