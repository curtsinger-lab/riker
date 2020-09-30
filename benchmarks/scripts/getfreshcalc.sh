#!/bin/sh

git clone https://github.com/lcn2/calc.git
cd calc
cp /home/dbarowy/Documents/Code/dodo/benchmarks/calc/Werror.patch .
git apply Werror.patch
