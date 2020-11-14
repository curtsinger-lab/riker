# This test checks whether we correctly handle artifacts left behind
# from a previous build, in this case, symlinks.

Run an initial build

Move to test directory
  $ cd $TESTDIR

Setup
  $ rm -rf .dodo a_file a_link Dodofile
  $ cp Dodofile-1 Dodofile

Run dodo
  $ $DODO --show
  dodo-launch
  Dodofile
  touch a_file
  ln -s a_file a_link

Check the output
  $ stat a_link
    File: a_link -> a_file
  .*symbolic link.* (re)
  Device.* (re)
  Access:.* (re)
  Access:.* (re)
  Modify:.* (re)
  Change:.* (re)
   Birth:.* (re)

Run a rebuild, which should do nothing, and more importantly, not die
  $ $DODO --show

Clean up
  $ rm -rf .dodo a_file a_link Dodofile
