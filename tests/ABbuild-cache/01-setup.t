This is just a caching version of the ABbuild test.

Move to test directory
  $ cd $TESTDIR

Restore input files
  $ cp file_versions/inputA_original inputA
  $ cp file_versions/inputB_original inputB

Remove output file
  $ rm -f myfile

Mark build file as executable
  $ chmod u+x Dodofile

Remove any leftover build database
  $ rm -rf .dodo
