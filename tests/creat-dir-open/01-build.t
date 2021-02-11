The open syscall with both the O_CREAT and O_DIRECTORY flags
set has unspecified behavior (wrt POSIX), and on Linux
does not actually imply that a directory is created. This test
ensures that we adhere to Linux behavior.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo creat-dir-open outcome
  $ clang creat-dir-open.c -o creat-dir-open
  $ umask 002

Run the first build
  $ $DODO --show
  dodo-launch
  Rikerfile
  ./creat-dir-open

Check the contents of a_file
  $ stat outcome
    File: outcome
  .*regular empty file.* (re)
  Device.* (re)
  Access: (0770/-rwxrwx---) * (glob)
  Access:.* (re)
  Modify:.* (re)
  Change:.* (re)
   Birth:.* (re)

Remove outcome
  $ rm outcome

Run the second build
  $ $DODO --show

Clean up
  $ rm -rf .dodo creat-dir-open outcome
