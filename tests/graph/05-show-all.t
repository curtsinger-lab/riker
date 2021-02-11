Move to test directory
  $ cd $TESTDIR

Clean up any previous build
  $ rm -rf .rkr output out.dot

Run a build
  $ $RKR

Generate graph output in unrendered dot format
  $ $RKR graph --no-render

Check the graph source for /bin/sh. We should not find it.
  $ grep -oE "/bin/(da)?sh" out.dot
  [1]

Generate graph output again, this time including all files
  $ $RKR graph -a --no-render

Check the graph source for /bin/sh. Now it should be there.
  $ grep -oE "/bin/(da)?sh" out.dot
  /bin/(da)?sh (re)

Clean up
  $ rm -rf .rkr output out.dot
