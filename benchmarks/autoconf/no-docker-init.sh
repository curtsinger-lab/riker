#!/bin/sh -x

BENCHMARK_ROOT=$1
BENCHMARK_NAME=$2

# make benchmark directory
mkdir -p $BENCHMARK_ROOT/$BENCHMARK_NAME

# change dir
cd $BENCHMARK_ROOT

# put runners in $BENCHMARK_ROOT
cp ../benchmarks/scripts/dodo-run.sh ../benchmarks/scripts/make-run.sh .

# obtain benchmark
wget --no-clobber https://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
tar xzvf autoconf-2.69.tar.gz -C autoconf --strip-components 1
cp ../benchmarks/autoconf/Rikerfile autoconf/

# go back
cd ..