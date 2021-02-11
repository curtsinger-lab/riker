Move to test directory
  $ cd $TESTDIR

Clean up any previous build
  $ rm -rf .rkr output out.png

Run a build
  $ $RKR

Generate graph output
  $ $RKR graph

Check for the rendered build graph
  $ file out.png
  out.png: PNG image .* (re)

Clean up
  $ rm -rf .rkr output out.png
