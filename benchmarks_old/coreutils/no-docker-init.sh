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
wget --no-clobber https://ftp.gnu.org/gnu/coreutils/coreutils-8.32.tar.gz
tar xzvf coreutils-8.32.tar.gz
mv coreutils-8.32 $BENCHMARK_NAME

# copy Rikerfile
cp ../benchmarks/coreutils/Rikerfile $BENCHMARK_NAME

# go back
cd ..