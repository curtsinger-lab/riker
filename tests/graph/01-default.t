Move to test directory
  $ cd $TESTDIR

Is graphviz installed? If not, skip this test
  $ which dot > /dev/null || exit 80

Clean up any previous build
  $ rm -rf .rkr output out.png

Run a build
  $ rkr

Generate graph output
  $ rkr graph

Check for the rendered build graph
  $ file out.png
  out.png: PNG image .* (re)

Clean up
  $ rm -rf .rkr output out.png
