#!/usr/bin/python

import os
import sys
from subprocess import Popen, PIPE
from threading import Thread
import shutil

# script configuration
BENCH_NAME = "calc"
BENCH_DIR = "/home/dbarowy/Documents/Code/calc"
DODO_DIR = "/home/dbarowy/Documents/Code/dodo"
DODO_DB  = DODO_DIR + "/.dodo"
DODO_EXE = DODO_DIR + "/dodo"
MAKE_EXE = "/usr/bin/make"
CLEAN_EXE = BENCH_DIR + "/clean.sh"
CSV_LOG = "/home/dbarowy/Documents/Code/benchmarks.csv"

# constants
DODO_BUILD = [DODO_EXE, "build"]
DODO_STATS = [DODO_EXE, "stats", "--csv"]
CLEAN_CMD  = [CLEAN_EXE]
ON_POSIX = 'posix' in sys.builtin_module_names

# runs a command, printing output as it runs
def rt_run_command(command):
    process = Popen(command, stdout=PIPE)
    while True:
        output = process.stdout.readline()
        if not output:
            break
        print(output.decode('utf-8').strip())
    rc = process.poll()
    return rc

# runs a command, buffering output as it runs
def run_command(command):
    s = ""
    process = Popen(command, stdout=PIPE)
    while True:
        output = process.stdout.readline()
        if not output:
            break
        s += output.decode('utf-8').strip()
    rc = process.poll()
    return (rc, s)

# cd to benchmark
os.chdir(BENCH_DIR)

# ensure a clean dodo build
print(">>> CLEANING BUILD ...")
shutil.rmtree(DODO_DB, ignore_errors=True)
rc = rt_run_command(CLEAN_CMD)
if rc != 0:
    print(">>> ERROR: Unable to clean build.")
    sys.exit(1)

# run dodo build & redirect stderr to stdout
print(">>> RUNNING BENCHMARK '" + BENCH_NAME + "' ...")
rc = rt_run_command(DODO_BUILD)
if rc != 0:
    print(">>> ERROR: Unable to run benchmark '" + BENCH_NAME + "'.")
    sys.exit(1)

# get stats
print(">>> OBTAINING STATS ...")
rc, csv = run_command(DODO_STATS)
if rc != 0:
    print(">>> ERROR: Unable to obtain build statistics for benchmark '" + BENCH_NAME + "'.")
    sys.exit(1)

# write stats to CSV; prepend benchmark name
with open(CSV_LOG, "a") as csv_log:
    csv_log.write("\"" + BENCH_NAME + "\"," + csv + "\n")

# tell the user that we are finished
print(">>> DONE: " + BENCH_NAME)