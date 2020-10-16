#!/bin/sh

COMBOS=131072
DODO=../../../dodo
DODOARGS=--debug --log trace,ir

# ensure that initial directory is clean
rm -rf test

# build stress binary
clang -Wall stress.c -o stress

# hammer dodo
for i in `seq 0 $COMBOS`;
do
    echo "FILE-DOESN'T-EXIST TEST: $i"
    mkdir test
    cd test
    $DODO $DODOARGS --args $i > log-$i.txt 2>&1
    cd ..
    rm -rf test
    
    echo "FILE-EXISTS TEST: $i"
    mkdir test
    cd test
    touch file
    $DODO $DODOARGS --args $i > log-$i.txt 2>&1
    cd ..
    rm -rf test

    echo "DIRECTORY-EXISTS TEST: $i"
    mkdir test
    cd test
    mkdir file
    $DODO $DODOARGS --args $i > log-$i.txt 2>&1
    cd ..
    rm -rf test

    echo "SYMLINK-EXISTS TEST: $i"
    mkdir test
    cd test
    touch phile
    ln -s phile file
    $DODO $DODOARGS --args $i > log-$i.txt 2>&1
    cd ..
    rm -rf test
done

# cleanup
rm -rf test stress
