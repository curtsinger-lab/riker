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

Recreate the input, but add an extra file
  $ mkdir foo
  $ touch foo/a
  $ touch foo/b
  $ touch foo/c

Run a rebuild
  $ $RKR --show
  rmdir foo
  rmdir: failed to remove 'foo': Directory not empty
  Rikerfile

The foo directory should be left over
  $ rm foo/c
  $ rmdir foo

Clean up
  $ rm -rf .rkr foo
