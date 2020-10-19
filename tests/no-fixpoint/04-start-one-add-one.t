Run a build that reads the contents of the current directory and creates a new file.
Start with one matching file in the directory, then add one and then run a rebuild.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo *.num *.err
  $ touch 0.num

Run the first build (note that commands in (re) below race)
  $ $DODO --show
  dodo-launch
  Dodofile
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  touch 1.num

Now create an additional file, which changes an input to the build
  $ touch 2.num

Run a rebuild (note that commands in (re) below race)
  $ $DODO --show
  Dodofile
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  ((ls -1)|(grep \.num)|(wc -l)) (re)
  touch 3.num

Run an additional rebuild, which should do nothing
  $ $DODO --show

Clean up
  $ rm -rf .dodo *.num
