This test checks to see if dodo does the right thing with an openat
call with the O_CREAT|O_EXCL|O_PATH flags set. Because the file already
exists, dodo should predict EEXIST and not crash.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo stress file core
  $ clang stress.c -o stress
  $ touch file

Run a build
  $ $DODO --show
  dodo-launch
  sh Dodofile
  ./stress 8328
  O_CREAT|O_EXCL|O_PATH (2097344)

Does 'file' exist?
  $ stat file
    File: file
  .*regular empty file.* (re)
  Device.* (re)
  Access:.* (re)
  Access:.* (re)
  Modify:.* (re)
  Change:.* (re)
   Birth: -

Clean up
  $ rm -rf .dodo stress file core

SKIP! This tests triggers both a mismatch between the openat() result and our model and causes a failed assert leading to a coredump.
  We need to fix the handling for open("file", O_CREAT|O_EXCL|O_PATH, <whatever>) when "file" is a regular empty file.
