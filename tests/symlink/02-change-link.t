Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo
  $ rm -f output dir/output

Make sure dir/link is a symlink to dir/..
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

Now change the link to refer to dir/.
  $ rm dir/link
  $ ln -s . dir/link

Run a rebuild
  $ $DODO --show
  cat input1
  cat input2
  cat input3

Check the output
  $ cat output
  This is a test
  And here's a third test

  $ cat dir/output
  This is another test

Put the link back how it was
  $ unlink dir/link
  $ ln -s .. dir/link

Clean up
  $ rm -rf .dodo 
  $ rm -f output dir/output
