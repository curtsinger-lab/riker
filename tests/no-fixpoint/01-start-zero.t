Run a build that reads the contents of the current directory and creates a new file.
Start with no matching files in the directory, then verify that a rebuild does no work.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo *.num

Run the first build (note that commands in (re) below race)
  $ $DODO --show
  dodo-launch
  Rikerfile
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  touch 0.num

Run the second build-- since nothing WAS changed, nothing SHOULD change
  $ $DODO --show

Clean up
  $ rm -rf .dodo *.num
