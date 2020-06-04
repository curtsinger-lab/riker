Move to test directory
  $ cd $TESTDIR

Prepare for a clean build
  $ rm -rf .dodo
  $ rm -f hello

Build with dodo. We don't expect any output on the first build
  $ $DODO 2>/dev/null

Run the simple program
  $ ./hello
  Hello world
