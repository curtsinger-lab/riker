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
  $ $DODO --show
  rmdir foo
  rmdir: failed to remove 'foo': Directory not empty
  (warning) [Command Rikerfile] should rerun because child [Command rmdir foo] changed exit status.
  (warning) [Command Rikerfile] should rerun because child [Command rmdir foo] changed exit status.

The foo directory should be left over
  $ rm foo/c
  $ rmdir foo

Clean up
  $ rm -rf .dodo foo
