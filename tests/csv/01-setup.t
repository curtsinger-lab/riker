Move to test directory
  $ cd $TESTDIR

Cleanup
  $ rm -rf myfile stats.csv .rkr

Run riker
  $ $RKR --show --stats stats.csv
  rkr-launch
  Rikerfile
  touch myfile

Look at the first column of the output
  $ awk -F "," '{ print $1 }' stats.csv
  "phase"
  "0"
  "1"
  "2"

Run a rebuild
  $ $RKR --show --stats stats.csv

Look at the first column of the output again
  $ awk -F "," '{ print $1 }' stats.csv
  "phase"
  "0"
  "1"
  "2"
  "0"
  "1"

Cleanup
  $ rm -rf myfile stats.csv .rkr
