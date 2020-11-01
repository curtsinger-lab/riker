#!/usr/bin/python3

import os
import sys
from subprocess import Popen, PIPE, check_output
from threading import Thread
from resource import *
import shutil
import json

# constants
ON_POSIX = 'posix' in sys.builtin_module_names

## FUNCTION DEFINITIONS

class Config:
    def __init__(self, confpath, outputpath, data):
        self.benchmark_name = data["name"]
        self.benchmark_path = os.path.dirname(os.path.realpath(confpath))
        self.runner_path    = os.path.dirname(os.path.realpath(__file__))
        self.dodo_path      = os.path.dirname(self.runner_path)
        self.dodo_exe       = os.path.join(self.dodo_path, "dodo")
        self.dodo_database  = os.path.join(self.benchmark_path, ".dodo")
        self.make_exe       = check_output(["which", "make"]).decode('utf-8').strip()
        self.clean_exe      = os.path.join(self.benchmark_path, data["clean"])
        self.tmpfile        = os.path.join(self.benchmark_path, data["tmp_csv"])
        self.output_csv     = os.path.realpath(outputpath)

    def __str__(self):
        return ("Configuration: \n"
                "\tbenchmark name:\t{}\n" +
                "\tbenchmark path:\t{}\n" +
                "\trunner path:\t{}\n" +
                "\tdodo path:\t{}\n" +
                "\tdodo exe:\t{}\n" +
                "\tdodo database:\t{}\n" +
                "\tmake exe:\t{}\n" +
                "\tclean exe:\t{}\n" +
                "\ttemporary csv:\t{}\n" +
                "\toutput csv:\t{}").format(
                    self.benchmark_name,
                    self.benchmark_path,
                    self.runner_path,
                    self.dodo_path,
                    self.dodo_exe,
                    self.dodo_database,
                    self.make_exe,
                    self.clean_exe,
                    self.tmpfile,
                    self.output_csv)

    def build_cmd(self):
        return [self.dodo_exe, "build", "--stats=" + self.tmpfile]
    
    def clean_cmd(self):
        return [self.clean_exe]

# read configuration
def init_config(args):
    if len(args) != 3:
        print("Usage:")
        print("\t" + args[0] + " <config file> <output.csv>")
        sys.exit(1)
    try:
        with open(args[1], 'r') as conf:
            return Config(args[1], args[2], json.load(conf))
    except OSError:
        print("Cannot read config file '" + args[1] + "'")
        sys.exit(1)

# removes a file, and doesn't complain if it doesn't exist
def rm_silently(file):
    try:
        os.remove(file)
    except OSError:
        pass

# runs a command, with optional updated environment
# variables, printing output as it runs.
def run_command(command, env={}):
    # obtain a copy of the current environment
    cur_env = os.environ.copy()

    # override using supplied variables
    cur_env.update(env)

    # call the process, with modified environment
    process = Popen(command, stdout=PIPE)
    while True:
        output = process.stdout.readline()
        if not output:
            break
        print(output.decode('utf-8').strip())
    rc = process.poll()
    return rc

# if csv does not exist, create file and write header;
# otherwise append
def csv_append(file, header, rows):
    # case: file does not exist (needs header)
    try:
        with open(file, "x") as csv_log:
            csv_log.write(header)
            for row in rows:
                csv_log.write(row)
    # case: file exists (no header)
    except IOError:
        with open(file, "a") as csv_log:
            for row in rows:
                csv_log.write(row)

# read a dodo --stats CSV file
def dodo_csv_read(file):
    with open(file, 'r') as fh:
        i = 0
        header = ""
        rows = []
        for line in fh.readlines():
            if i == 0:
                header = line
            else:
                rows[i-1] = line
        (header, rows)

## MAIN METHOD

# init config
conf = init_config(sys.argv)

# cd to benchmark
os.chdir(conf.benchmark_path)

# ensure a clean dodo build
print(">>> CLEANING BUILD ...")
rm_silently(conf.tmpfile)
shutil.rmtree(conf.dodo_database, ignore_errors=True)
rc = run_command(conf.clean_cmd())
if rc != 0:
    print(">>> ERROR: Unable to clean build.")
    sys.exit(1)

# run dodo --stats build & redirect stderr to stdout
print(">>> RUNNING BENCHMARK '" + conf.name + "' ...")
rc = run_command(conf.build_cmd())
if rc != 0:
    print(">>> ERROR: Unable to run benchmark '" + conf.name + "'.")
    sys.exit(1)

# read temporary CSV
(header, rows) = dodo_csv_read(conf.tmpfile)

# write stats to CSV output; prepend benchmark name
csv_append(conf.output_csv, header, rows)

# tell the user that we are finished
print(">>> DONE: " + dodo.name)