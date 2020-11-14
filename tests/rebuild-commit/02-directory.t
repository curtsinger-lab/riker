# This test checks whether we correctly handle artifacts left behind
# from a previous build, in this case, symlinks.

Run an initial build

Move to test directory
  $ cd $TESTDIR

Setup
  $ rm -rf .dodo a_dir Dodofile
  $ cp Dodofile-2 Dodofile

Run dodo
  $ $DODO --show
  dodo-launch
  Dodofile
  mkdir a_dir

Check the output
  $ stat a_dir
    File: a_dir
  .*directory.* (re)
  Device.* (re)
  Access:.* (re)
  Access:.* (re)
  Modify:.* (re)
  Change:.* (re)
   Birth:.* (re)

Run a rebuild, which should do nothing, and more importantly, not die
  $ $DODO --show

Clean up
  $ rm -rf .dodo a_dir Dodofile
