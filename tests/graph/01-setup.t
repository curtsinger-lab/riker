Move to test directory
  $ cd $TESTDIR

Remove output file
  $ rm -f myfile

Mark build file as executable
  $ chmod u+x Dodofile

Remove any leftover build database
  $ rm -rf .dodo

Remove any leftover graph output
  $ rm -rf out.dot out.png
