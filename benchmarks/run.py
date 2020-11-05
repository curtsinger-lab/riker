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

# constants
ON_POSIX = 'posix' in sys.builtin_module_names

## FUNCTION DEFINITIONS

class Config:
    def __init__(self, confpath, outputpath, cleanup, data):
        # validation
        required_keys = ["name", "docker_runner", "tmp_csv", "image_version"]
        for key in required_keys:
            if not key in data:
                "'{}' must contain key '{}'".format(confpath, key)
                sys.exit(1)

        # read values
        self.benchmark_name = data["name"]
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
        self.do_cleanup     = cleanup

    def __str__(self):
        return ("Configuration: \n"
                "\tremove image after running:\t{}\n"
                "\tbenchmark name:\t{}\n"
                "\tbenchmark path:\t{}\n"
                "\tbenchmark root:\t{}\n"
                "\trunner path:\t{}\n"
                "\tdodo path:\t{}\n"
                "\tdodo exe:\t{}\n"
                "\tdodo database:\t{}\n"
                "\tmake exe:\t{}\n"
                "\ttemporary csv:\t{}\n"
                "\toutput csv:\t{}\n"
                "\tdocker exe:\t{}\n"
                "\timage version:\t{}\n"
                "\tDockerfile:\t{}\n"
                "\tdocker runner:\t{}\n"
                "\ttime data csv:\t{}").format(
                    "yes" if self.do_cleanup else "no",
                    self.benchmark_name,
                    self.benchmark_path,
                    self.benchmark_root,
                    self.runner_path,
                    self.dodo_path,
                    self.dodo_exe,
                    self.dodo_database,
                    self.make_exe,
                    self.tmpfile,
                    self.output_csv,
                    self.docker_exe,
                    self.image_version,
                    self.dockerfile,
                    self.docker_runner,
                    self.time_data_csv
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

    def docker_containers_cmd(self):
        return [self.docker_exe,
                "ps",
                "--format={{.ID}}:{{.Image}}:{{.Names}}"
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
        (rc, rv) = run_command_capture(self.docker_containers_cmd())
        if rc == 0:
            first = True
            for line in rv.splitlines():
                rx = r"(?P<container_id>[^\s]+):(?P<image_id>[^\s]+):(?P<container_name>[^\s]+)"
                p = re.compile(rx, re.IGNORECASE)
                m = p.search(line)
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
            print("ERROR: Something went wrong.")
            sys.exit(1)

    # start docker image
    def start_container(self):
        rc = run_command(self.docker_run_container_cmd())
        if rc != 0:
            print("ERROR: Something went wrong.")
            sys.exit(1)

    # run benchmark
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
            print("ERROR: Something went wrong.")
            sys.exit(1)

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
    # note that this does not remove the image, so that
    # if you decide to run the benchmark again, it will
    # already have a built image and be able to run again
    # quickly.
    # To delete the image too, supply the --cleanup flag
    def rm(self, rm_image):
        rc = run_command(conf.docker_stop_container_cmd())
        if rc != 0:
            print("ERROR: Unable to stop container '{}'.".format(conf.docker_container_name()))
            # don't die, just return so that results can be
            # written out later
            return
        rc = run_command(conf.docker_rm_container_cmd())
        if rc != 0:
            print("ERROR: Unable to remove stopped container '{}'".format(conf.docker_container_name()))
            # don't die; see above
            return
        if rm_image:
            rc = run_command(conf.docker_rm_image_cmd())
            if rc != 0:
                print("ERROR: Unable to remove image '{}'".format(conf.docker_image_fullname()))
                # again, don't die
        return

# read configuration
def init_config(args):
    parser = argparse.ArgumentParser(description="Runs a benchmark given a JSON configuration file "
                                                 "and a CSV for output.  If the CSV already exists, "
                                                 "this program appends output to it.")
    parser.add_argument("config", help="path to a JSON benchmark configuration file")
    parser.add_argument("output", help="path to a CSV for benchmark output")
    parser.add_argument("-c", "--cleanup", help="shut down and remove Docker image after running", action="store_true")
    pargs = parser.parse_args()

    try:
        with open(pargs.config, 'r') as conf:
            return Config(pargs.config, pargs.output, pargs.cleanup, json.load(conf))
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
                csv_log.write(row)
    # case: file exists (no header)
    except IOError:
        with open(file, "a") as csv_log:
            for row in rows:
                csv_log.write(row)

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
        rows += "\"" + column[i] + "\"," + csv_rows[i]
    return (h2, rows)


## MAIN METHOD

# init config
conf = init_config(sys.argv)
print(conf)

# initialize docker container, if necessary
if not conf.image_is_initialized():
    print("INFO: Docker image '{}' is not initialized.  Initializing...".format(conf.docker_image_fullname()))
    conf.initialize_docker_image()
else:
    print("INFO: Docker image '{}' is already initialized.  Skipping initialization.".format(conf.docker_image_fullname()))

# start docker image, if necessary
if not conf.container_is_running():
    print("INFO: Docker container '{}' is not running.  Starting...".format(conf.docker_container_name()))
    conf.start_container()
else:
    print("INFO: Docker container '{}' is already running.  Skipping startup.".format(conf.docker_container_name()))

# run benchmark
conf.exec_benchmark()

# copy outputs to 'local' machine
dodo_stats_tmpfile = conf.copy_docker_file(conf.tmpfile)
dodo_time_tmpfile = conf.copy_docker_file(conf.time_data_csv)

# merge csvs and return as (header, rows)
(header, rows) = merge_csvs(dodo_stats_tmpfile, dodo_time_tmpfile)

# add benchmark name
(header, rows) = prepend_column("benchmark_name", [conf.benchmark_name], header, rows)

# write out results
csv_append(conf.output_csv, header, rows)

# teardown docker containers
conf.rm(conf.do_cleanup)

# tell the user that we are finished
print("INFO: DONE: " + conf.benchmark_name)
print("INFO: RESULTS APPENDED TO: " + conf.output_csv)
