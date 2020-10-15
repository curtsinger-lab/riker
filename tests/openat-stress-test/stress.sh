#!/usr/bin/bash

COMBOS=131072
for i in `seq 0 $COMBOS`;

do
    echo "FILE DOESN't EXIST TEST: $i"
    mkdir test
    cd test
    ../stress $i
    cd ..
    rm -rf test
    
    echo "FILE EXISTS TEST: $i"
    mkdir test
    cd test
    touch file
    ../stress $i
    cd ..
    rm -rf test
done

