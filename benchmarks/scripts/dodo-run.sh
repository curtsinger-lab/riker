#!/bin/sh -x

cd $BENCHMARK_ROOT/$BENCHMARK_NAME

# just run dodo
$DODO_EXE --stats $TIME_CSV
