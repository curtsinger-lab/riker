Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr foo
  $ mkdir foo
  $ touch foo/a
  $ touch foo/b

Run the first build
  $ rkr --show
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

Run a rebuild. The `rm foo/a` and `rm foo/b` commands are emulated and committed, but then `rmdir foo` fails. That forces a rerun of `Rikerfile`. When re-emulating `rm foo/a` and `rm foo/b` these commands both have changed predicates and must rerun. They also change error codes and force a second run of Rikerfile.
  $ rkr --show
  rmdir foo
  rmdir: failed to remove 'foo': Directory not empty
  Rikerfile
  rm foo/a
  rm: cannot remove 'foo/a': No such file or directory
  rm foo/b
  rm: cannot remove 'foo/b': No such file or directory
  Rikerfile

The end result is equivalent to running ./Rikerfile, so this seems acceptable. It may be possible to skip the re-execution of `rm foo/a` and `rm foo/b`, but it's not obvious how. The `rmdir foo` exit code change couldn't impact `rm foo/a` or `rm foo/b` because both commands run entirely before `rmdir foo` is called, so maybe that helps.

It may also work to generate post-build predicates on each iteration of the build, but only for commands that did not observe a change. That way `rm foo/a` and `rm foo/b` would have post-build predicates that reflect their effect on the environment.

The foo directory should be left over
  $ rm foo/c
  $ rmdir foo

Clean up
  $ rm -rf .rkr foo
