Move to test directory
  $ cd $TESTDIR

Clean up any previous build
  $ rm -rf .dodo output out.dot out.png

Run a build
  $ $DODO

Generate graph output in unrendered dot format
  $ $DODO graph --no-render

Now render the graph with graphviz
  $ dot -Tpng out.dot > out.png

Check the final rendered output
  $ file out.png
  out.png: PNG image.* (re)

Clean up
  $ rm -rf .dodo output out.dot out.png
