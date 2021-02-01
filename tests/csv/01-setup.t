Move to test directory
  $ cd $TESTDIR

Cleanup
  $ rm -rf myfile stats.csv .dodo

Run dodo
  $ $DODO --show --stats stats.csv
  dodo-launch
  Dodofile
  touch myfile

Look at the first column of the output
  $ awk -F "," '{ print $1 }' stats.csv
  "phase"
  "0"
  "1"
  "2"

Run a rebuild
  $ $DODO --show --stats stats.csv

Look at the first column of the output again
  $ awk -F "," '{ print $1 }' stats.csv
  "phase"
  "0"
  "1"
  "2"
  "0"
  "1"
  "2"

Cleanup
  $ rm -rf myfile stats.csv .dodo
