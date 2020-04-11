Move to test directory
  $ cd $TESTDIR

Prepare for a clean build
  $ rm -rf .dodo
  $ rm -f hello
  $ cp file_versions/hello-original.c hello.c

Build with dodo. We don't expect any output on the first build
  $ ../../dodo 2> /dev/null

Run the simple program
  $ ./hello
  Hello world
