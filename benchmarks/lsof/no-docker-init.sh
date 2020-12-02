#!/bin/sh -x

BENCHMARK_ROOT = $1
BENCHMARK_NAME = $2

# make benchmark directory
mkdir -p $BENCHMARK_ROOT/$BENCHMARK_NAME

# change dir
cd $BENCHMARK_ROOT

# put runners in $BENCHMARK_ROOT
cp ../scripts/dodo-run.sh ../scripts/make-run.sh .

# obtain benchmark
git clone https://github.com/lsof-org/lsof.git
cp ../lsof/Dodofile lsof/

# go back
cd ..