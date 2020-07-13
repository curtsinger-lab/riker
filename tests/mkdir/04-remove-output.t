Run an initial build, remove the output file, then rebuild

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run. Create an empty output file for now, so rebuilding works
  $ rm -rf .dodo output
  $ echo "Hello" > input
  $ touch output

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  mkdir foo
  cat input
  mv foo/f output
  rmdir foo

Check the output
  $ cat output
  Hello

Remove the output file
  $ rm output

Run a rebuild
  $ $DODO --show
  cat input
  mv foo/f output

Check the output
  $ cat output
  Hello

Run an additional rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output
  Hello

Clean up
  $ rm -rf .dodo foo
  $ rm output

SKIP! This test is not working. The root cause is unclear, but here's the issue:
The `mv` command that runs as part of the rebuild depends on whether or not `output` exists. On the first rebuild it does not, but on the second rebuild it does exist. On the second rebuild, Dodo tries to rerun just the `mv` command, which depends on the file `foo/f`, which we do not have saved.
The fix is to properly encode the dependency on the contents of `foo/f`. The first fix I attempted was to add a canLink method to Artifact. This method checked to make sure the artifact was in a state that could be linked: it was either on the filesystem or had a version that could be committed. But for `foo/f`, the file is not linkable because the created version was already replaced with the output from `cat`.
The real issue seems to be that there is no dependency on the contents of `foo/f`. What is the right time to report the dependency on this file's contents?
  $ exit 80
