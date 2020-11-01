#!/usr/bin/python3

import os
import sys
import pwd
from subprocess import Popen, PIPE, check_output
from threading import Thread
from resource import *
import shutil
import json
import re

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
        self.docker_exe     = check_output(["which", "docker"]).decode('utf-8').strip()
        self.dockerfile     = os.path.join(self.benchmark_path, "Dockerfile")
        self.image_version  = int(data["image_version"])

    def __str__(self):
        return ("Configuration: \n"
                "\tbenchmark name:\t{}\n"
                "\tbenchmark path:\t{}\n"
                "\trunner path:\t{}\n"
                "\tdodo path:\t{}\n"
                "\tdodo exe:\t{}\n"
                "\tdodo database:\t{}\n"
                "\tmake exe:\t{}\n"
                "\tclean exe:\t{}\n"
                "\ttemporary csv:\t{}\n"
                "\toutput csv:\t{}\n"
                "\tdocker exe:\t{}\n"
                "\timage version:\t{}\n"
                "\tDockerfile:\t{}").format(
                    self.benchmark_name,
                    self.benchmark_path,
                    self.runner_path,
                    self.dodo_path,
                    self.dodo_exe,
                    self.dodo_database,
                    self.make_exe,
                    self.clean_exe,
                    self.tmpfile,
                    self.output_csv,
                    self.docker_exe,
                    self.image_version,
                    self.dockerfile
                    )

    def build_cmd(self):
        return [self.dodo_exe, "build", "--stats=" + self.tmpfile]
    
    def clean_cmd(self):
        return [self.clean_exe]

    def docker_images_cmd(self):
        return [self.docker_exe, "images"]

    def username(self):
        return pwd.getpwuid(os.getuid())[0]

    def docker_image_name(self):
        return self.username() + "/benchmark-" + self.benchmark_name

    def docker_image_version(self):
        return "v" + str(self.image_version)

    def docker_image_fullname(self):
        return self.docker_image_name() + ":" + self.docker_image_version()

    def docker_initialize_cmd(self):
        return [self.docker_exe,                                # docker
                "build",                                        # build an image
                "-f={}".format(self.dockerfile),                # location of dockerfile
                "-t={}".format(self.docker_image_fullname()),     # name of the image
                "../"                                           # working directory for build
                ]

    # returns true if image already set up
    def image_is_initialized(self):
        (rc, rv) = run_command_capture(self.docker_images_cmd())
        if (rc == 0):
            first = True
            for line in rv.splitlines():
                if first:
                    first = False
                    continue
                rx = rf"(?P<repository>[^\s]+)\s+(?P<tag>[^\s]+)\s+(?P<image_id>[0-9a-z]+)\s+(?P<created>.+)\s+(?P<size>[0-9.]+.B)"
                p = re.compile(rx, re.IGNORECASE)
                m = p.search(line)
                if (m.group("repository") == self.docker_image_name()) and (m.group("tag") == self.docker_image_version()):
                    return True
            return False
        else:
            print("Unable to query docker ({}) for image data.".format(self.docker_exe))
            sys.exit(1)

    # initializes a docker image
    def initialize_docker_image(self):
        rc = run_command(self.docker_initialize_cmd())
        if rc != 0:
            print("Something went wrong.")
            sys.exit(1)

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
# returns a return code.
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

# runs a command, with optional updated environment
# variables, saving output to a string as it runs.
# returns a return code and the output string
def run_command_capture(command, env={}):
    # obtain a copy of the current environment
    cur_env = os.environ.copy()

    # override using supplied variables
    cur_env.update(env)

    # initialize empty stdout string
    s = ""

    # call the process, with modified environment
    process = Popen(command, stdout=PIPE)
    while True:
        output = process.stdout.readline()
        if not output:
            break
        s += output.decode('utf-8')
    rc = process.poll()
    return (rc, s)

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
print(conf)

# initialize docker container, if necessary
if not conf.image_is_initialized():
    print("Docker image '{}' is not initialized.  Initializing...".format(conf.docker_image_fullname()))
    conf.initialize_docker_image()
else:
    print("Docker image '{}' is already initialized.  Skipping initialization.".format(conf.docker_image_fullname()))

sys.exit(0)

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
print(">>> RUNNING BENCHMARK '" + conf.benchmark_name + "' ...")
rc = run_command(conf.build_cmd())
if rc != 0:
    print(">>> ERROR: Unable to run benchmark '" + conf.benchmark_name + "'.")
    sys.exit(1)

# read temporary CSV
(header, rows) = dodo_csv_read(conf.tmpfile)

# write stats to CSV output; prepend benchmark name
csv_append(conf.output_csv, header, rows)

# tell the user that we are finished
print(">>> DONE: " + dodo.benchmark_name)
