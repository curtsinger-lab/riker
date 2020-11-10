#!/usr/bin/python3

import os
import sys
import pwd
from subprocess import Popen, PIPE, check_output
from resource import *
import shutil
import json
import re
import argparse
import tempfile
import time

# constants
ON_POSIX = 'posix' in sys.builtin_module_names

## FUNCTION DEFINITIONS

class Configs:
    def __init__(self, confpaths, outputpath, start_clean, cleanup, dontask, no_changes_rb):
        self.configs = []
        self.output_csv = outputpath
        self.dont_ask = dontask
        
        # process each config
        for confpath in confpaths:
            # read config JSON and add to list
            with open(confpath, 'r') as conf:
                self.configs += [Config(confpath, outputpath, start_clean, cleanup, no_changes_rb, json.load(conf))]

    def __str__(self):
        s = ""
        
        for conf in self.configs:
            s += "======= CONFIGURATION: {} ==========================\n".format(conf.benchmark_name)
            s += str(conf)
        
        s += "All output will be written to: {}\n".format(os.path.realpath(self.output_csv))
        return s

class Config:
    def __init__(self, confpath, outputpath, start_clean, cleanup, no_changes_rb, data):
        # validation
        required_keys = ["name", "docker_runner", "tmp_csv", "image_version"]
        for key in required_keys:
            if not key in data:
                "'{}' must contain key '{}'".format(confpath, key)
                sys.exit(1)

        # read values
        self.benchmark_name  = data["name"]
        self.benchmark_path = os.path.dirname(os.path.realpath(confpath))
        self.benchmark_root = data["benchmark_root"]
        self.runner_path    = os.path.dirname(os.path.realpath(__file__))
        self.dodo_path      = os.path.dirname(self.runner_path)
        self.dodo_exe       = os.path.join(self.dodo_path, "dodo")
        self.dodo_database  = os.path.join(self.benchmark_path, ".dodo")
        self.make_exe       = check_output(["which", "make"]).decode('utf-8').strip()
        self.tmpfile        = os.path.join(self.benchmark_path, data["tmp_csv"])
        self.output_csv     = os.path.realpath(outputpath)
        self.docker_exe     = check_output(["which", "docker"]).decode('utf-8').strip()
        self.dockerfile     = os.path.join(self.benchmark_path, "Dockerfile")
        self.image_version  = int(data["image_version"])
        self.docker_runner  = data["docker_runner"]
        self.time_data_csv  = data["time_data_csv"]
        self.start_clean    = start_clean
        self.do_cleanup     = cleanup
        self.no_changes_rb  = no_changes_rb

    def __str__(self):
        return ("\tcleanup before running:\t{}\n"
                "\tcleanup after running:\t{}\n"
                "\tbenchmark path:\t\t{}\n"
                "\tbenchmark root:\t\t{}\n"
                "\trunner path:\t\t{}\n"
                "\tdodo path:\t\t{}\n"
                "\tdodo exe:\t\t{}\n"
                "\tdodo database:\t\t{}\n"
                "\tmake exe:\t\t{}\n"
                "\ttemporary csv:\t\t{}\n"
                "\tdocker exe:\t\t{}\n"
                "\timage version:\t\t{}\n"
                "\tDockerfile:\t\t{}\n"
                "\tdocker runner:\t\t{}\n"
                "\ttime data csv:\t\t{}\n"
                "\trebuild (no changes)\t{}\n").format(
                    "yes" if self.start_clean else "no",
                    "yes" if self.do_cleanup else "no",
                    self.benchmark_name,
                    self.benchmark_root,
                    self.runner_path,
                    self.dodo_path,
                    self.dodo_exe,
                    self.dodo_database,
                    self.make_exe,
                    self.tmpfile,
                    self.docker_exe,
                    self.image_version,
                    self.dockerfile,
                    self.docker_runner,
                    self.time_data_csv,
                    self.no_changes_rb
                    )

    def build_cmd(self):
        return [self.dodo_exe, "build", "--stats=" + self.tmpfile]
    
    def clean_cmd(self):
        return [self.clean_exe]

    def username(self):
        return pwd.getpwuid(os.getuid())[0]

    def docker_container_name(self):
        return "benchmark-" + self.benchmark_name

    def docker_image_name(self):
        return self.username() + "/" + self.docker_container_name()

    def docker_image_version(self):
        return "v" + str(self.image_version)

    def docker_image_fullname(self):
        return self.docker_image_name() + ":" + self.docker_image_version()

    def docker_images_cmd(self):
        return [self.docker_exe, "images"]

    def docker_containers_cmd(self, filters=[]):
        flts = ""
        if len(filters) > 0:
            fs = map(lambda x: "--filter {}".format(x), filters)
            flts = " ".join(fs)
            return [self.docker_exe,
                "ps",                                                   # show running containers
                "-a",                                                   # even non-running ones
                flts,                                                   # add in some filters
                "--format={{.ID}}:{{.Image}}:{{.Names}}:{{.Status}}"    # in this format
                ]
        return [self.docker_exe,
                "ps",                                                   # show running containers
                "-a",                                                   # even non-running ones
                "--format={{.ID}}:{{.Image}}:{{.Names}}:{{.Status}}"    # in this format
                ]

    def docker_initialize_cmd(self):
        return [self.docker_exe,                                # docker
                "build",                                        # build an image
                "-f={}".format(self.dockerfile),                # location of dockerfile
                "-t={}".format(self.docker_image_fullname()),   # name of the image
                "../"                                           # working directory for build
                ]
            
    def docker_run_container_cmd(self):
        # docker run --security-opt seccomp=unconfined --name benchmark-calc -dit dbarowy/benchmark-calc:v1
        return [self.docker_exe,                                            # docker
                'run',                                                      # run an image
                '--security-opt seccomp=unconfined',                        # enable ptrace 
                '--name {}'.format(self.docker_container_name()),           # container name
                '-dit {}'.format(self.docker_image_fullname())              # image name
                ]

    def docker_exec_benchmark_cmd(self, env={}):
        # docker exec -e {environment=variables} benchmark-calc /benchmark/run.sh
        if len(env) > 0:
            env_strings = []
            for key in env:
                env_strings += ["-e {}='{}'".format(key, env[key])]

            arr = []
            arr += [self.docker_exe, "exec"]
            arr += env_strings                                                  # environment variables
            arr += ["{}".format(self.docker_container_name()),                  # the name of the running container
                    "{}".format(self.docker_runner)                             # the path to the program in the container
                    ]
            return arr
        else:
            return [self.docker_exe,                                            # docker
                    "exec",                                                     # run a program inside a container
                    "{}".format(self.docker_container_name()),                  # the name of the running container
                    "{}".format(self.docker_runner)                             # the path to the program in the container
                    ]

    def docker_stop_container_cmd(self):
        return [self.docker_exe,                                            # docker
                'stop',                                                     # stop container
                '{}'.format(self.docker_container_name()),                  # container name
                ]

    def docker_rm_container_cmd(self):
        return [self.docker_exe,                                            # docker
                'rm',                                                       # remove container
                '{}'.format(self.docker_container_name()),                  # container name
                ]

    def docker_rm_image_cmd(self):
        return [self.docker_exe,                                            # docker
                'rmi',                                                      # remove image
                '{}'.format(conf.docker_image_fullname()),                  # image name
                ]

    # copy a file in a docker container to a local file
    def docker_cp_file_cmd(self, docker_file, local_file):
        return [self.docker_exe,
                "cp",
                "{}:{}".format(self.docker_container_name(), docker_file),
                local_file
                ]

    # returns true if image already set up
    def image_is_initialized(self):
        (rc, rv) = run_command_capture(self.docker_images_cmd())
        if rc == 0:
            first = True
            for line in rv.splitlines():
                if first:
                    first = False
                    continue
                rx = r"(?P<repository>[^\s]+)\s+(?P<tag>[^\s]+)\s+(?P<image_id>[0-9a-z]+)\s+(?P<created>.+)\s+(?P<size>[0-9.]+.B)"
                p = re.compile(rx, re.IGNORECASE)
                m = p.search(line)
                if (m.group("repository") == self.docker_image_name()) and (m.group("tag") == self.docker_image_version()):
                    return True
            return False
        else:
            print("ERROR: Unable to query docker ({}) for image data (return code: {}).".format(self.docker_exe, rc))
            sys.exit(1)

    # returns true if container is running
    def container_is_running(self):
        if not self.image_is_initialized():
            print("ERROR: Docker image '{}' is not initialized".format(self.docker_image_fullname()))
            sys.exit(1)
        (rc, rv) = run_command_capture(self.docker_containers_cmd(["status=running"]))
        if rc == 0:
            first = True
            for line in rv.splitlines():
                rx = r"(?P<container_id>[^\s]+):(?P<image_id>[^\s]+):(?P<container_name>[^\s]+):(?P<status>.+)"
                p = re.compile(rx, re.IGNORECASE)
                m = p.search(line)
                if (m.group("container_name") == self.docker_container_name()):
                    return True
            return False
        else:
            print("ERROR: Unable to query docker ({}) for container data (return code: {}).".format(self.docker_exe, rc))
            sys.exit(1)

    # returns true if container is dead
    def container_is_dead(self):
        if not self.image_is_initialized():
            print("ERROR: Docker image '{}' is not initialized".format(self.docker_image_fullname()))
            sys.exit(1)
        (rc, rv) = run_command_capture(self.docker_containers_cmd(["status=dead", "status=exited"]))
        if rc == 0:
            first = True
            for line in rv.splitlines():
                rx = r"(?P<container_id>[^\s]+):(?P<image_id>[^\s]+):(?P<container_name>[^\s]+):(?P<status>.+)"
                p = re.compile(rx, re.IGNORECASE)
                m = p.search(line)
                status = m.group("status")
                if (m.group("container_name") == self.docker_container_name()):
                    return True
            return False
        else:
            print("ERROR: Unable to query docker ({}) for container data (return code: {}).".format(self.docker_exe, rc))
            sys.exit(1)

    # initializes a docker image
    def initialize_docker_image(self):
        rc = run_command(self.docker_initialize_cmd())
        if rc != 0:
            print("ERROR: Unable to create docker image.")
            sys.exit(1)

    # start docker image
    def start_container(self):
        rc = run_command(self.docker_run_container_cmd())
        if rc != 0:
            print("ERROR: Unable to start docker container.")
            sys.exit(1)

    # run benchmark
    # returns whatever return code was returned by the benchmark
    def exec_benchmark(self):
        if not self.container_is_running():
            print("ERROR: Cannot run benchmark without a running docker image.")
            sys.exit(1)

        # run benchmark script
        my_env = {
            "BENCHMARK_NAME": self.benchmark_name,
            "BENCHMARK_ROOT": self.benchmark_root,
            "TIME_CSV": self.time_data_csv,
            "TMP_CSV": self.tmpfile
        }
        rc = run_command(self.docker_exec_benchmark_cmd(my_env))
        if rc != 0:
            print("ERROR: Benchmark returned {}.".format(rc))
        return rc

    # copies the given file from the running docker container
    # to a new tempfile and returns the name of that tempfile
    def copy_docker_file(self, file):
        (_, t) = tempfile.mkstemp()
        args = self.docker_cp_file_cmd(file, t)
        rc = run_command(args)
        if rc != 0:
            print("ERROR: Unable to copy file '{}' from docker.".format(file))
        return t

    # stop container and remove it
    # deletes the image too when rm_image is true
    # when ignore_failure is true, just keep chugging along even
    # if commands fail
    def rm(self, rm_image, ignore_failure = False):
        rc = run_command_capture(conf.docker_stop_container_cmd(), suppress_printing=ignore_failure)
        if rc != 0 and not ignore_failure:
            print("ERROR: Unable to stop container '{}'.".format(conf.docker_container_name()))
            # don't die, just return so that results can be
            # written out later
            return
        rc = run_command_capture(conf.docker_rm_container_cmd(), suppress_printing=ignore_failure)
        if rc != 0 and not ignore_failure:
            print("ERROR: Unable to remove stopped container '{}'".format(conf.docker_container_name()))
            # don't die; see above
            return
        if rm_image and not ignore_failure:
            rc = run_command_capture(conf.docker_rm_image_cmd(), suppress_printing=ignore_failure)
            if rc != 0:
                print("ERROR: Unable to remove image '{}'".format(conf.docker_image_fullname()))
                # again, don't die
        return

# read configuration
def init_configs(args):
    parser = argparse.ArgumentParser(description="Runs a benchmark given at least one JSON "
                                                 "configuration file and a CSV for output.  If the "
                                                 "CSV already exists, this program appends output "
                                                 "to it. If more than one config is supplied, by "
                                                 "default, this program will wait for the user to "
                                                 "confirm that they actually want to run all of "
                                                 "given benchmarks.")
    parser.add_argument("output", help="path to a CSV for benchmark output")
    parser.add_argument("configs",
                        metavar="config",
                        nargs="+",
                        help="path to a JSON benchmark configuration file")
    parser.add_argument("--cleanup-before",
                        help="remove conflicting container and image before running",
                        action="store_true")
    parser.add_argument("--cleanup-after",
                        help="shut down and remove Docker image after running",
                        action="store_true")
    parser.add_argument("--dont-ask",
                        help="don't ask for user confirmation, just run",
                        action="store_true")
    parser.add_argument("--incr-none",
                        help="measure incremental rebuild time for no changes",
                        action="store_true")
    pargs = parser.parse_args()

    try:
        return Configs(pargs.configs, pargs.output, pargs.cleanup_before, pargs.cleanup_after, pargs.dont_ask, pargs.incr_none)
    except OSError:
        print("ERROR: Cannot read config file '" + pargs.config + "'")
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
    (rc, _) = run_command_capture(command, env, suppress_printing=False)
    return rc

# runs a command, with optional updated environment
# variables, saving output to a string as it runs.
# returns a return code and the output string
def run_command_capture(command, env={}, suppress_printing=True):
    # mash args into a string, and use shell=True
    # because Python does not correctly process arguments
    # containing equals signs.
    args = " ".join(command)

    # obtain a copy of the current environment
    cur_env = os.environ.copy()

    # override using supplied variables
    cur_env.update(env)

    # initialize empty stdout string
    output = ""

    # call the process, with modified environment
    process = Popen(args, stdout=PIPE, shell=True, env=cur_env)
    cap = ""
    while True:
        output = process.stdout.readline()
        if not output:
            break
        s = output.decode('utf-8')
        cap += s
        if not suppress_printing:
            print(s.strip())
    rc = process.poll()
    return (rc, cap)

# if csv does not exist, create file and write header;
# otherwise append
def csv_append(file, header, rows):
    # case: file does not exist (needs header)
    try:
        with open(file, "x") as csv_log:
            csv_log.write(header + "\n")
            for row in rows:
                csv_log.write(row + "\n")
    # case: file exists (no header)
    except IOError:
        with open(file, "a") as csv_log:
            for row in rows:
                csv_log.write(row + "\n")

# read a CSV file, returning
# a tuple containing the header and an array of rows
def csv_read(file):
    with open(file, 'r') as fh:
        header = ""
        first = True
        rows = []
        for line in fh.readlines():
            if first:
                header = line.strip()
                first = False
            else:
                rows += [line.strip()]
        return (header, rows)

# reads two csv files and returns the result
# merged in the form of a (header, rows)
def merge_csvs(file1, file2):
    (h1, rs1) = csv_read(file1)
    (h2, rs2) = csv_read(file2)
    header = h1 + "," + h2
    assert len(rs1) == len(rs2)
    rows = []
    for i, _ in enumerate(rs1):
        rows += [rs1[i] + "," + rs2[i]]
    return (header, rows)

# prepend the column with the given header
# and data to the CSV represented as a (header, rows)
def prepend_column(header, column, csv_header, csv_rows):
    # add the new column to the header
    h2 = "\"" + header + "\"," + csv_header

    # add the data to the rows
    assert len(column) == len(csv_rows)
    rows = []
    for i, _ in enumerate(csv_rows):
        rows += ["\"" + column[i] + "\"," + csv_rows[i]]
    return (h2, rows)

# runs the configured benchmark, and depending on whether
# additional runs are required (e.g., rebuild), appends
# a note to the outputted benchmark name in the CSV
def run_benchmark(conf, header_note=""):
     # run benchmark
    rc = conf.exec_benchmark()

    header = "" # we don't know what the header is yet
    rows = [""] # nor the rows

    # if there was no error, copy CSV data
    if rc == 0:
        # copy outputs to 'local' machine
        dodo_stats_tmpfile = conf.copy_docker_file(conf.tmpfile)
        dodo_time_tmpfile = conf.copy_docker_file(conf.time_data_csv)

        # merge csvs and return as (header, rows)
        (header, rows) = merge_csvs(dodo_stats_tmpfile, dodo_time_tmpfile)
    
    # record the return code-- is nonzero in case of error
    (header, rows) = prepend_column("return_code", [str(rc)], header, rows)

    # add benchmark name
    (header, rows) = prepend_column("benchmark_name", [conf.benchmark_name + header_note], header, rows)

    return (header, rows)

## MAIN METHOD

# init config
c = init_configs(sys.argv)
print(c)
if len(c.configs) > 1 and not c.dont_ask:
    yn = input("DO YOU WANT TO CONTINUE? [Y/n] ")
    if yn != "" and yn != "Y" and yn != "y":
        sys.exit(0)

for conf in c.configs:
    # if the user asked us to start with a clean slate, do so
    if conf.start_clean:
        print("INFO: Pre-cleaning docker images...".format(conf.start_clean))
        conf.rm(True, True)

    # initialize docker container, if necessary
    if not conf.image_is_initialized():
        print("INFO: Docker image '{}' is not initialized.  Initializing...".format(conf.docker_image_fullname()))
        conf.initialize_docker_image()
    else:
        print("INFO: Docker image '{}' is already initialized.  Skipping initialization.".format(conf.docker_image_fullname()))

    # start docker image, if necessary
    if conf.container_is_dead():
        print("INFO: Docker container '{}' is dead.  Removing old container and restarting...".format(conf.docker_container_name()))
        conf.rm(False)
        time.sleep(2)    # wait a little bit for the container to go away
        conf.start_container()
    elif not conf.container_is_running():
        print("INFO: Docker container '{}' is not running.  Starting...".format(conf.docker_container_name()))
        conf.start_container()
    else:
        print("INFO: Docker container '{}' is already running.  Skipping startup.".format(conf.docker_container_name()))

    # run benchmark and obtain CSV result
    (header, rows) = run_benchmark(conf)

    # write out results
    csv_append(conf.output_csv, header, rows)

    # rebuild with no changes?
    if conf.no_changes_rb:
        # run benchmark and obtain CSV result
        (header, rows) = run_benchmark(conf, "-rebuild_no_changes")

        # write out results
        csv_append(conf.output_csv, header, rows)

    # tear down docker containers
    conf.rm(conf.do_cleanup)

    # tell the user that we are finished
    print("INFO: DONE: " + conf.benchmark_name)
    print("INFO: RESULTS APPENDED TO: " + conf.output_csv)
