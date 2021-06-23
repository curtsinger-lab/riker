This test checks to see if riker does the right thing with an openat
call with the O_CREAT|O_EXCL|O_PATH flags set. Because the file already
exists, riker should predict EEXIST and not crash.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr stress file core
  $ clang stress.c -o stress
  $ touch file

Run a build
  $ rkr --show
  rkr-launch
  sh Rikerfile
  ./stress 8328
  O_CREAT|O_EXCL|O_PATH (2097344)

Does 'file' exist?
  $ stat file
    File: file
  .*regular empty file.* (re)
  Device:.* (re)
  Access:.* (re)
  Access:.* (re)
  Modify:.* (re)
  Change:.* (re)
   Birth:.* (re)

Clean up
  $ rm -rf .rkr stress file core
