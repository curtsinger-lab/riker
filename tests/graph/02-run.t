Move to test directory
  $ cd $TESTDIR

Run the build
  $ ../../dodo --show
  dodo-launch
  Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ cat myfile
  hello world

Run the build again, doing nothing this time
  $ ../../dodo --show

Generate a build graph
  $ ../../dodo graph

Render the graph
  $ dot -Tpng out.dot > out.png
