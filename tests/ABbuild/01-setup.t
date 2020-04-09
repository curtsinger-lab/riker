Move to test directory
  $ cd $TESTDIR

Restore inputB file
  $ cp file_versions/inputB_original inputB

Remove output file
  $ rm -f myfile

Remove any leftover build database
  $ rm -rf .dodo
