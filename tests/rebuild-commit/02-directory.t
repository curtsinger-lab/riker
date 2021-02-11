# This test checks whether we correctly handle artifacts left behind
# from a previous build, in this case, symlinks.

Run an initial build

Move to test directory
  $ cd $TESTDIR

Setup
  $ rm -rf .rkr a_dir Rikerfile
  $ cp Rikerfile-2 Rikerfile

Run riker
  $ $RKR --show
  rkr-launch
  Rikerfile
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
  $ $RKR --show

Clean up
  $ rm -rf .rkr a_dir Rikerfile
