#!/bin/sh

# this is just a convenient script to run all of the benchmarks;
# the benchmark runner can already handle running multiple
# programs, but the syntax is a tad fiddly

BENCHMARKS=`find . -iname "benchmark.json" -print`
FLAGS=--cleanup-before --cleanup-after --incr-none-dodo --incr-none-make --dont-ask

# run in Docker
./run.py $FLAGS $1 $BENCHMARKS

# run outside of Docker
./run.py $FLAGS --no-docker $1 $BENCHMARKS
