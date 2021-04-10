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

Recreate the input, but leave out the b file
  $ mkdir foo
  $ touch foo/a

Run a rebuild. The `rm foo/b` command has to run because `foo/b` no longer exists. Its exit code changes, forcing a rerun of `Rikerfile`.
  $ $RKR --show
  rm foo/b
  rm: cannot remove 'foo/b': No such file or directory
  Rikerfile

Make sure the foo directory does not exist
  $ stat foo
  stat: cannot statx? 'foo': No such file or directory (re)
  [1]

Clean up
  $ rm -rf .rkr foo
