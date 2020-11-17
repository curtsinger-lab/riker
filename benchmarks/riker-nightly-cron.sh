#!/bin/sh

This script requires two email addresses on the command line.  E.g.,
$ riker-nightly-cron.sh dan@example.com charlie@example.com

SCRIPTPATH=/riker-nightly
LOGPATH=${SCRIPTPATH}/logs

# clone/pull repository if needed
if [ ! -d /riker-nightly/dodo ]
then
    cd /riker-nightly
    git clone --recursive git@github.com:curtsinger-lab/dodo.git
else
    cd /riker-nightly/dodo
    git pull
fi

# get current date & time
NOW=`date +"%Y-%m-%d_%H-%M"`

# run benchmarks
# the following is for debugging purposes-- runs one benchmark
#cd /riker-nightly/dodo/benchmarks && ./run.py ${NOW}-output.csv lsof/benchmark.json 1> ${NOW}-stdout.txt 2> ${NOW}-stderr.txt
# the following runs all benchmarks
cd /riker-nightly/dodo/benchmarks && ./run-all.sh ${LOGPATH}/${NOW}-output.csv 1> ${LOGPATH}/${NOW}-stdout.txt 2> ${LOGPATH}/${NOW}-stderr.txt

# mail the output
echo "Benchmark results attached." | /usr/bin/mail -s "Riker nightly benchmark results" -A ${LOGPATH}/${NOW}-output.csv $1,$2

