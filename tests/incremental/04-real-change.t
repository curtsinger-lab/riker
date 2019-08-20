Move to test directory
  $ cd $TESTDIR

Change the output message
  $ cp file_versions/hello-message.c hello.c

Rebuild
  $ ../../dodo
  .*/cc1 .* (re)
  .*-as .* (re)
  rm .*\.s (re)
  .*/collect2 .* (re)
  Unrecognized process ended: [0-9]+ (re)

Check output
  $ ./hello
  Goodbye world


