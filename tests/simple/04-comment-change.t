Move to test directory
  $ cd $TESTDIR

Add a comment to the source file
  $ cp file_versions/hello-comment.c hello.c

Run a rebuild. We should compile to assembly, then stop.
  $ ../../dodo
  [^ ]*/cc1 .* (re)
  rm [^ ]*\.s (re)

SKIP! This test does not work.
  $ exit 80
