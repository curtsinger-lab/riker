#!/bin/sh -x

cd $BENCHMARK_ROOT/$BENCHMARK_NAME
/usr/bin/time --output=$TIME_FILE ../../dodo/dodo --stats $TMP_CSV
