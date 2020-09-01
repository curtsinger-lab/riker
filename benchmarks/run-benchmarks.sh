#!/bin/sh

## RUN

# calc
docker run --name benchmark-calc -dit dbarowy/benchmark-calc:v1
docker exec benchmark-calc /benchmark/run.sh

