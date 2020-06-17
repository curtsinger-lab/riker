Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo
  $ rm -f output dir/output

Make sure dir/link is a symlink to ..
  $ rm -f dir/link
  $ ln -s .. dir/link

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  cat input1
  cat input2
  cat input3

Make sure both cats wrote to the same output file
  $ cat output
  This is another test
  And here's a third test

Run a rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat output
  This is another test
  And here's a third test

Clean up
  $ rm -rf .dodo output
