#!/bin/sh -x

BENCHMARK_ROOT=$1
BENCHMARK_NAME=$2

# make benchmark directory
mkdir -p $BENCHMARK_ROOT

# change dir
cd $BENCHMARK_ROOT

# put runners in $BENCHMARK_ROOT
cp ../benchmarks/scripts/dodo-run.sh ../benchmarks/scripts/make-run.sh .

# obtain benchmark
wget --no-clobber https://riker-benchmarks.s3.amazonaws.com/linux-5.9.1.tar.xz
tar xf linux-5.9.1.tar.xz
rm linux-5.9.1.tar.xz
mv linux-5.9.1 $BENCHMARK_NAME
cp ../benchmarks/linux/linux-kernel.config $BENCHMARK_NAME/.config

# copy Rikerfile
cp ../benchmarks/linux/Rikerfile $BENCHMARK_NAME

# go back
cd ..
