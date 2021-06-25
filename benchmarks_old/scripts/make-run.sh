#!/bin/sh

cd $BENCHMARK_ROOT/$BENCHMARK_NAME

# delete the file if it already exists
rm -f $TIME_CSV

# create file containing just the header, if necessary
echo '"phase", "emulated_commands", "traced_commands", "emulated_steps", "traced_steps", "artifacts", "versions", "ptrace_stops", "syscalls", "elapsed_ns"' > $TIME_CSV

# compute elapsed time in nanoseconds (only works on Linux)
time_start=$(date +%s%N)
./Rikerfile
time_end=$(($(date +%s%N) - $time_start))

echo "0,0,0,0,0,0,0,0,0,${time_end}" >> $TIME_CSV
