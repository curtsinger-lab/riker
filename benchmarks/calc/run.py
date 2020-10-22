#!/usr/bin/python

import os
import sys
from subprocess import Popen, PIPE
from threading import Thread
from resource import *
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
HEADER = ("benchmark,"
          "user_time_sec,"
          "sys_time_sec,"
          "max_rss_kb,"
          "num_block_iops,"
          "num_block_oops,"
          "num_commands,"
          "num_traced_commands,"
          "sum_emulated_commands,"
          "num_steps,"
          "num_emulated_steps,"
          "num_artifacts,"
          "num_versions\n")
# rusage fields
UTIME = 0       # time in user mode (float seconds)
STIME = 1       # time in system mode (float seconds)
MAXRSS = 2      # maximum resident set size (KB)
IXRSS = 3       # shared memory size (unused on Linux)
IDRSS = 4       # unshared memory size (unused on Linux)
ISRSS = 5       # unshared stack size (unused on Linux)
MINFLT = 6      # page faults not requiring I/O
MAJFLT = 7      # page faults requiring I/O
NSWAP = 8       # number of swap outs (unused on Linux)
INBLOCK = 9     # block input operations
OUTBLOCK = 10   # block output operations
MSGSND = 11     # messages sent (unused on Linux)
MSGRCV = 12     # messages received (unused on Linux)
NSIGNALS = 13   # signals received (unused on Linux)
NVCSW = 14      # voluntary context switches
NIVCSW = 15     # involuntary context switches

# borrowed from: https://stackoverflow.com/a/40344234
class ThreadWithReturnValue(Thread):
    def __init__(self, group=None, target=None, name=None, args=(), kwargs=None, *, daemon=None):
        Thread.__init__(self, group, target, name, args, kwargs, daemon=daemon)
        self._return = None

    def run(self):
        if self._target is not None:
            self._return = self._target(*self._args, **self._kwargs)

    def join(self):
        Thread.join(self)
        return self._return

# runs a command, printing output as it runs
# also collects program resource usage stats
def rt_run_command(command):
    # inner function runs the subprocess
    def r():
        process = Popen(command, stdout=PIPE)
        while True:
            output = process.stdout.readline()
            if not output:
                break
            print(output.decode('utf-8').strip())
        rc = process.poll()
        return (rc, getrusage(RUSAGE_THREAD))

    # running in a thread allows us to scope getrusage to a specific thread
    t = ThreadWithReturnValue(target = r, args = ())
    t.start()
    return t.join()

# runs a command, buffering output as it runs
def run_command(command):
    s = ""
    process = Popen(command, stdout=PIPE, stderr=sys.stdout.buffer)
    while True:
        output = process.stdout.readline()
        if not output:
            break
        s += output.decode('utf-8').strip()
    rc = process.poll()
    return (rc, s)

# if csv does not exist, create file and write header;
# otherwise append
def csv_append(file, rusage, s):
    row = ("\"" + BENCH_NAME +
          "\",\"" + str(rusage[UTIME]) +
          "\",\"" + str(rusage[STIME]) +
          "\",\"" + str(rusage[MAXRSS]) +
          "\",\"" + str(rusage[INBLOCK]) +
          "\",\"" + str(rusage[OUTBLOCK]) +
          "\",\"" + s + "\n")
    try:
        with open(file, "x") as csv_log:
            csv_log.write(HEADER)
            csv_log.write(row)
    except IOError:
        with open(file, "a") as csv_log:
            csv_log.write(row)

# cd to benchmark
os.chdir(BENCH_DIR)

# ensure a clean dodo build
print(">>> CLEANING BUILD ...")
shutil.rmtree(DODO_DB, ignore_errors=True)
rc, _ = rt_run_command(CLEAN_CMD)
if rc != 0:
    print(">>> ERROR: Unable to clean build.")
    sys.exit(1)

# run dodo build & redirect stderr to stdout
print(">>> RUNNING BENCHMARK '" + BENCH_NAME + "' ...")
rc, rusage = rt_run_command(DODO_BUILD)
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
csv_append(CSV_LOG, rusage, csv)

# tell the user that we are finished
print(">>> DONE: " + BENCH_NAME)