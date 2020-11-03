#!/bin/sh -x

cd $BENCHMARK_ROOT/$BENCHMARK_NAME

# -f is:
# e     elapsed wall clock time, in seconds
# S     cpu-seconds of system time
# U     cpu-seconds of user time
# P     percent CPU time
# t     average resident set size in kb

echo "wall_s,system_s,user_s,pct_cpu,avg_rss" >> $TIME_CSV
/usr/bin/time -f"%e,%S,%U,%P,%t" --output=$TIME_CSV --append ../../dodo/dodo --stats $TMP_CSV
