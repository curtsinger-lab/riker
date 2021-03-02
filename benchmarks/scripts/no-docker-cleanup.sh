#!/bin/sh -x

BENCHMARK_ROOT=$1
BENCHMARK_NAME=$2

# remove benchmark directory
rm -rf $BENCHMARK_ROOT/$BENCHMARK_NAME
