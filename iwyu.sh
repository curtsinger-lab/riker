#!/bin/sh
make -k -j1 CXX="iwyu -Xiwyu --mapping_file=.iwyu.imp"