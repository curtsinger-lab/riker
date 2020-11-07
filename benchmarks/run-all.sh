#!/bin/sh

# this is just a convenient script to run all of the benchmarks;
# the benchmark runner can already handle running multiple
# programs, but the syntax is a tad fiddly

BENCHMARKS=`find . -iname "benchmark.json" -print`

NOW=`date +"%Y-%m-%d_%H-%M"`

./run.py --dont-ask output-${NOW}.csv ${BENCHMARKS}
