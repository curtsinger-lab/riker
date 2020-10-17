Run a build that reads the contents of the current directory and creates a new file.
Start with no matching files in the directory, add one, then do a rebuild.

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

SKIP! This test fails because `grep` never writes to the pipe that `wc` reads. When we plan a rebuild, we never mark `wc` for rerun.
  $ exit 80
