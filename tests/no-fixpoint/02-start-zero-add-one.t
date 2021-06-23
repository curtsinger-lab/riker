Run a build that reads the contents of the current directory and creates a new file.
Start with no matching files in the directory, add one, then do a rebuild.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr *.num

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  touch 0.num

Now create an additional file, which changes an input to the build
  $ touch 1.num

Run a rebuild
  $ rkr --show
  Rikerfile
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  touch 2.num

Run an additional rebuild, which should do nothing
  $ rkr --show

Clean up
  $ rm -rf .rkr *.num
