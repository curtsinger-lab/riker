Move to test directory
  $ cd $TESTDIR

Clean up build targets
  $ make clean > /dev/null

Restore inputB file
  $ cp file_versions/inputB_original inputB

Build the test programs
  $ make > /dev/null

Remove a leftover Dodo database
  $ rm -f db.dodo
