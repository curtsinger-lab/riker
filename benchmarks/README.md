# Riker Benchmarks HOWTO

Riker benchmarks are distributed as a set of `Dockerfile`s.  When run on Linux, Docker overhead is relatively low, so we use it as a convenient way of running benchmarks without having to install any software locally.  Note that we strongly recommend running these containers on a Linux machine, as Docker overhead for I/O is unacceptably high on the MacOS and Windows.

Each benchmark is installed in its own separate Docker container, and all of the benchmarks follow the same general format.

## Initial Setup

To setup all of the benchmarks, run the `setup_benchmarks.sh` script found in the `benchmarks` folder.

## Running an Individual Benchmark

To run a selected benchmark, first ensure that the benchmark container has been setup (see above) and then run:

```
TODO
```

## Running All Benchmarks