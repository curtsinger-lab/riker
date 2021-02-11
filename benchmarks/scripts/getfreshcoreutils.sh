#!/bin/sh

wget --no-clobber https://ftp.gnu.org/gnu/coreutils/coreutils-8.32.tar.gz
tar xzvf coreutils-8.32.tar.gz
cd coreutils-8.32
cp /home/dbarowy/Documents/Code/dodo/benchmarks/coreutils/Rikerfile .
