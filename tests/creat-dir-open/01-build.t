The open syscall with both the O_CREAT and O_DIRECTORY flags
set has unspecified behavior (wrt POSIX), and on Linux
does not actually imply that a directory is created. This test
ensures that we adhere to Linux behavior.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr creat-dir-open outcome
  $ clang creat-dir-open.c -o creat-dir-open
  $ umask 002

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  ./creat-dir-open

If the file was not created on the initial build, skip this test.
  $ test -f outcome || exit 80

Check the contents of a_file
  $ stat outcome
    File: outcome
  .*regular empty file.* (re)
  Device.* (re)
  Access:.* (re)
  Access:.* (re)
  Modify:.* (re)
  Change:.* (re)
   Birth:.* (re)

Remove outcome
  $ rm outcome

Run the second build
  $ rkr --show

Clean up
  $ rm -rf .rkr creat-dir-open outcome
