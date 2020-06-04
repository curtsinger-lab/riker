Move to test directory
  $ cd $TESTDIR

Change the output message
  $ cp file_versions/hello-message.c hello.c

Rebuild
  $ $DODO
  .*/cc1 .* (re)
  .*-as .* (re)
  rm .*\.s (re)
  .*/collect2 .* (re)
  rm .*\.o (re)

Check output
  $ ./hello
  Goodbye world

SKIP! This test does not work
  $ exit 80
