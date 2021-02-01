#!/bin/sh

cd $BENCHMARK_ROOT/$BENCHMARK_NAME

# create file containing just the header, if necessary
if [ ! -f "$TIME_CSV" ] ; then
    echo '"phase", "emulated_commands", "traced_commands", "emulated_steps", "traced_steps","artifacts", "versions", "ptrace_stops", "syscalls", "elapsed_ns"' > $TIME_CSV
fi

# compute elapsed time in nanoseconds (only works on Linux)
time_start=$(date +%s%N)
./Dodofile
time_end=$((($(date +%s%N) - $time_start)))

echo "0,0,0,0,0,0,0,0,0,${time_end}" >> $TIME_CSV