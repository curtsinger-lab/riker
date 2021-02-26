#!/usr/bin/python3

import json
import os
import sys

# this is just a convenient script to run all of the benchmarks;
# the benchmark runner can already handle running multiple
# programs, but the syntax is a tad fiddly

# Just put your list of benchmarks in a JSON file called "benchmarks.json."
# Paths will be resolved relative to the benchmarks.json file.

# FLAGS = "--cleanup-before --cleanup-after --incr-none-riker --incr-none-make --dont-ask"
FLAGS = "--cleanup-before --cleanup-after --incr-none-riker --incr-none-make --dont-ask --no-docker"

# process args
if len(sys.argv) != 3:
    print("Usage: run-all.py <benchmarks.json> <output.csv>")
    sys.exit(1)
arg_json = sys.argv[1]
arg_csvf = sys.argv[2]

# get directory of benchmarks.json file
base = os.path.abspath(os.path.dirname(arg_json))

# read JSON
with open(arg_json) as f:
    data = json.load(f)

    # run each benchmark
    for benchmark in data["benchmarks"]:
        # resolve benchmark relative to base dir
        path = os.path.join(base, benchmark)

        # first command
        cmd1 = ["./run.py", FLAGS, arg_csvf, path]

        # stringify command
        args1 = " ".join(cmd1)

        # call the process
        print(args1)
        os.system(args1)

        # second command
        cmd2 = ["./run.py", FLAGS, "--no-docker", arg_csvf, path]

        # stringify command
        args2 = " ".join(cmd2)

        # call the process
        print(args2)
        os.system(args2)
