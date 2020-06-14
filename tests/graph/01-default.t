Move to test directory
  $ cd $TESTDIR

Clean up any previous build
  $ rm -rf .dodo output out.png

Run a build
  $ $DODO

Generate graph output
  $ $DODO graph

Check for the rendered build graph
  $ file out.png
  out.png: PNG image .* (re)

Clean up
  $ rm -rf .dodo output out.png
