Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr foo
  $ mkdir foo
  $ touch foo/a
  $ touch foo/b

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  rm foo/a
  rm foo/b
  rmdir foo

Recreate the input
  $ mkdir foo
  $ touch foo/a
  $ touch foo/b

Run a rebuild
  $ $RKR --show

Make sure the foo directory does not exist
  $ stat foo
  stat: cannot statx? 'foo': No such file or directory (re)
  [1]

Clean up
  $ rm -rf .rkr foo
