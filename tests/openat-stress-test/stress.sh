#!/bin/sh -x

COMBOS=131072
RKR=../../../rkr
LOGDIR=logs-`date +"%Y-%m-%d"`

# ensure that initial directory is clean
rm -rf test stress

# ensure that log directory exists
mkdir -p $LOGDIR

# build stress binary
clang -Wall stress.c -o stress

# hammer rkr
for i in `seq 0 $COMBOS`;
do
    echo "FILE-DOESN'T-EXIST TEST: $i"
    mkdir test
    cp Rikerfile stress test/
    cd test
    rkr --debug --log trace,ir --args $i > ../$LOGDIR/log-$i-empty.txt 2>&1
    cd ..
    rm -rf test
    
    echo "FILE-EXISTS TEST: $i"
    mkdir test
    cp Rikerfile stress test/
    cd test
    touch file
    rkr --debug --log trace,ir --args $i > ../$LOGDIR/log-$i-file.txt 2>&1
    cd ..
    rm -rf test

    echo "DIRECTORY-EXISTS TEST: $i"
    mkdir test
    cp Rikerfile stress test/
    cd test
    mkdir file
    rkr --debug --log trace,ir --args $i > ../$LOGDIR/log-$i-dir.txt 2>&1
    cd ..
    rm -rf test

    echo "SYMLINK-EXISTS TEST: $i"
    mkdir test
    cp Rikerfile stress test/
    cd test
    touch phile
    ln -s phile file
    rkr --debug --log trace,ir --args $i > ../$LOGDIR/log-$i-symlink.txt 2>&1
    cd ..
    rm -rf test
done

# cleanup
rm -rf test stress
