#!/bin/sh -x

cd $BENCHMARK_ROOT/$BENCHMARK_NAME

# delete the file if it already exists
rm -f $TIME_CSV

# just run dodo
$RIKER_EXE --stats $TIME_CSV
