Move to test directory
  $ cd $TESTDIR

Prepare for a clean build
  $ rm -f db.dodo hello
  $ cp file_versions/hello-original.c hello.c

Build with dodo. We don't expect any output on the first build
  $ ../../dodo

Run the simple program
  $ ./hello
  Hello world
