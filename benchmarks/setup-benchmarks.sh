#!/bin/sh

## note that the Docker context is the dodo repository's root directory ##

## SETUP ##

# calc
docker build -f calc/Dockerfile -t dbarowy/benchmark-calc:v1 ../
