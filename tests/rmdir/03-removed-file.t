Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo foo
  $ mkdir foo
  $ touch foo/a
  $ touch foo/b

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  rm foo/a
  rm foo/b
  rmdir foo

Recreate the input, but leave out the b file
  $ mkdir foo
  $ touch foo/a

Run a rebuild
  $ $DODO --show
  rm foo/b
  rm: cannot remove 'foo/b': No such file or directory
  rmdir foo

Make sure the foo directory does not exist
  $ stat foo
  stat: cannot stat 'foo': No such file or directory
  [1]

Clean up
  $ rm -rf .dodo foo
