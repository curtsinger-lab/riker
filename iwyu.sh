#!/bin/sh
make -k -B CXX="iwyu -Xiwyu --mapping_file=.iwyu.imp"