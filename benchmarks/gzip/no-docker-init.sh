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
wget --no-clobber https://ftp.gnu.org/gnu/gzip/gzip-1.10.tar.gz
tar xzvf gzip-1.10.tar.gz
rm gzip-1.10.tar.gz
mv gzip-1.10 $BENCHMARK_NAME

# copy Rikerfile
cp ../benchmarks/gzip/Rikerfile $BENCHMARK_NAME

# go back
cd ..