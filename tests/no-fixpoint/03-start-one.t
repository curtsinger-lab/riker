Run a build that reads the contents of the current directory and creates a new file.
Start with one matching file in the directory, then verify that a rebuild does no work.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo *.num *.err
  $ touch 0.num

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  touch 1.num

Run the second build-- since nothing WAS changed, nothing SHOULD change
  $ $DODO --show

Clean up
  $ rm -rf .dodo *.num
