#!/bin/sh

wget --no-clobber https://ftp.gnu.org/gnu/gzip/gzip-1.10.tar.gz
tar xzvf gzip-1.10.tar.gz
rm gzip-1.10.tar.gz
cp dodo/benchmarks/gzip/Dodofile gzip-1.10/
