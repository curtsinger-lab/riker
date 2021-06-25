#!/bin/sh -x

BENCHMARK_ROOT=$1
BENCHMARK_NAME=$2

# make benchmark directory
mkdir -p $BENCHMARK_ROOT

# change dir
cd $BENCHMARK_ROOT

# put runners in $BENCHMARK_ROOT
cp ../benchmarks/scripts/dodo-run.sh ../benchmarks/scripts/make-run.sh ../benchmarks/scripts/no-docker-cleanup.sh .

# obtain benchmark
git clone https://github.com/lcn2/calc.git $BENCHMARK_NAME
cp ../benchmarks/calc/Werror.patch $BENCHMARK_NAME
cd $BENCHMARK_NAME
git apply Werror.patch

# copy Rikerfile
cp ../../benchmarks/calc/Rikerfile .

# go back
cd ../..