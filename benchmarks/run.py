#!/usr/bin/python3

from typing import *
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
from enum import Enum
import shutil

## TYPES
CSV = Tuple[str, List[str]]

## CONSTANTS
ON_POSIX = 'posix' in sys.builtin_module_names

## CLASS DEFINITIONS

class Tool(Enum):
    DODO = 1
    MAKE = 2

    def __str__(self) -> str:
        if self.value == 1:
            return "dodo"
        if self.value == 2:
            return "make"
        raise Exception("Unknown tool type.")

class Task(Enum):
    FULL = 1
    REBUILD_NO_CHANGES = 2

    def __str__(self) -> str:
        if self.value == 1:
            return "full"
        if self.value == 2:
            return "rebuild-no-changes"
        raise Exception("Unknown task type.")

class DockerStats:
    def __init__(self, input_bytes: float, output_bytes: float):
        self.input_bytes: float = input_bytes
        self.output_bytes: float = output_bytes

    def __str__(self):
        return ("Block input bytes: {}\n".format(self.input_bytes) +
                "Block output bytes: {}".format(self.output_bytes))

    @staticmethod
    def toBytes(count: str, unit: str) -> float:
        if unit == "B":
            return float(count)
        elif unit == "kB":
            return float(count) * 1024
        elif unit == "MB":
            return float(count) * 1024**2
        elif unit == "GB":
            return float(count) * 1024**3
        else:
            raise Exception("Unknown unit '{}'".format(unit))

class Configs:
    def __init__(self, pargs: argparse.Namespace):
        self.configs: List[Config]  = []
        self.output_csv: str        = pargs.output
        self.dont_ask: bool         = pargs.dont_ask
        
        # process each config
        for confpath in pargs.configs:
            # read config JSON and add to list
            with open(confpath, 'r') as conf:
                self.configs += [Config(pargs, confpath, json.load(conf))]

    def __str__(self) -> str:
        s: str = ""
        
        for conf in self.configs:
            s += "======= CONFIGURATION: {} ==========================\n".format(conf.benchmark_name)
            s += str(conf)
        
        s += "All output will be written to: {}\n".format(os.path.realpath(self.output_csv))
        return s

class Config:
    def __init__(self, pargs: argparse.Namespace, confpath: str, data: Dict[str,str]):

        # validation
        required_keys: List[str] = ["name", "docker_dodo_runner", "docker_make_runner", "tmp_csv", "image_version"]
        for key in required_keys:
            if not key in data:
                print("'{}' must contain key '{}'".format(confpath, key))
                sys.exit(1)

        # benchmark name
        self.benchmark_name: str      = data["name"]
        # benchmark path
        self.benchmark_path: str      = os.path.abspath(os.path.dirname(os.path.realpath(confpath)))
        # run outside of docker?
        self.no_docker                = pargs.no_docker
        # location of benchmark dir in Docker
        self.benchmark_root: str      = os.path.abspath(data["no_docker_root"]) if self.no_docker else data["benchmark_root"]
        # parent directory of this script
        self.my_path: str             = os.path.dirname(os.path.realpath(__file__))
        # location of dodo executable parent dir
        self.dodo_path: str           = os.path.dirname(self.my_path)
        # location of dodo executable
        self.dodo_exe: str            = os.path.join(self.dodo_path, "dodo") if self.no_docker else "/dodo/dodo"
        # location of make executable
        self.make_exe: str            = check_output(["which", "make"]).decode('utf-8').strip()
        # final output CSV file outside of Docker
        self.output_csv: str          = os.path.realpath(pargs.output)
        # location of Docker exectable
        self.docker_exe: str          = check_output(["which", "docker"]).decode('utf-8').strip()
        # location of Dockerfile
        self.dockerfile: str          = os.path.join(self.benchmark_path, "Dockerfile")
        # version name to use for container
        self.image_version: int       = int(data["image_version"])
        # location of script in Docker used to run dodo
        self.docker_dodo_runner: str  = data["docker_dodo_runner"]
        # location of script in Docker used to run make
        self.docker_make_runner: str  = data["docker_make_runner"]
        # remove container and image before running?
        self.cleanup_before: bool     = pargs.cleanup_before
        # remove container and image after running?
        self.cleanup_after: bool      = pargs.cleanup_after
        # run a dodo rebuild after a full build with no changes?
        self.incr_none_dodo: bool     = pargs.incr_none_dodo
        # run a make rebuild after a full build with no changes?
        self.incr_none_make: bool     = pargs.incr_none_make
        # run make?
        self.make: bool               = True if pargs.incr_none_make else pargs.make
        # run dodo?
        self.dodo: bool               = True if pargs.incr_none_dodo else pargs.dodo

        # more validation:
        if not self.make and not self.dodo:
            print("You must specify whether to run dodo (using --dodo or --incr-none-dodo) or "
                  "make (using --make or --incr-none-make)")
            sys.exit(1)

        # optional no-docker init script
        if "no_docker_init_script" in data:
            self.no_docker_init_script = os.path.join(self.benchmark_path, data["no_docker_init_script"])
        elif self.no_docker:
            raise Exception("Benchmark configuration in --no-docker mode must specify 'no_docker_init_script'.")
        # optional path to create no-docker benchmark
        if "no_docker_root" in data:
            self.no_docker_path = os.path.join(self.benchmark_root, self.benchmark_name)
        elif self.no_docker:
            raise Exception("Benchmark configuration in --no-docker mode must specify 'no_docker_root'.")
        # optional no-docker dodo script
        if "no_docker_dodo_runner" in data:
            self.no_docker_dodo_runner = os.path.abspath(os.path.join(self.my_path, data["no_docker_dodo_runner"]))
        elif self.no_docker:
            raise Exception("Benchmark configuration in --no-docker mode must specify 'no_docker_dodo_runner'.")
        # optional no-docker make script
        if "no_docker_make_runner" in data:
            self.no_docker_make_runner = os.path.abspath(os.path.join(self.my_path, data["no_docker_make_runner"]))
        elif self.no_docker:
            raise Exception("Benchmark configuration in --no-docker mode must specify 'no_docker_make_runner'.")
        # location for CSV output of time data
        self.time_data_csv: str = os.path.abspath(os.path.join(self.benchmark_root, data["time_data_csv"]))


    def __str__(self) -> str:
        return ("\trun inside Docker:\t\t{}\n"
                "\tcleanup before running:\t\t{}\n"
                "\tcleanup after running:\t\t{}\n"
                "\tbenchmark name:\t\t\t{}\n"
                "\tbenchmark root:\t\t\t{}\n"
                "\trun.py path:\t\t\t{}\n"
                "\tdodo path:\t\t\t{}\n"
                "\tdodo exe:\t\t\t{}\n"
                "\tmake exe:\t\t\t{}\n"
                "\tdocker exe:\t\t\t{}\n"
                "\timage version:\t\t\t{}\n"
                "\tDockerfile:\t\t\t{}\n"
                "\tdocker dodo runner:\t\t{}\n"
                "\tdocker make runner:\t\t{}\n"
                "\tno-docker dodo runner:\t\t{}\n"
                "\tno-docker make runner:\t\t{}\n"
                "\ttime data csv:\t\t\t{}\n"
                "\tclean build (with dodo):\t{}\n"
                "\tclean build (with make):\t{}\n"
                "\trebuild (no changes w/dodo)\t{}\n"
                "\trebuild (no changes w/make)\t{}\n"
                "\tno-docker init script\t\t{}\n"
                "\tno-docker path\t\t\t{}\n"
                ).format(
                    "yes" if not self.no_docker else "no",
                    "yes" if self.cleanup_before else "no",
                    "yes" if self.cleanup_after else "no",
                    self.benchmark_name,
                    self.benchmark_root,
                    self.my_path,
                    self.dodo_path,
                    self.dodo_exe,
                    self.make_exe,
                    self.docker_exe,
                    self.image_version,
                    self.dockerfile,
                    self.docker_dodo_runner,
                    self.docker_make_runner,
                    self.no_docker_dodo_runner,
                    self.no_docker_make_runner,
                    self.time_data_csv,
                    "yes" if self.dodo else "no",
                    "yes" if self.make else "no",
                    "yes" if self.incr_none_dodo else "no",
                    "yes" if self.incr_none_make else "no",
                    self.no_docker_init_script,
                    self.no_docker_path
                    )

    def username(self) -> str:
        return pwd.getpwuid(os.getuid())[0]

    def docker_container_name(self) -> str:
        return "benchmark-" + self.benchmark_name

    def docker_image_name(self) -> str:
        return self.username() + "/" + self.docker_container_name()

    def docker_image_version(self) -> str:
        return "v" + str(self.image_version)

    def docker_image_fullname(self) -> str:
        return self.docker_image_name() + ":" + self.docker_image_version()

    def docker_images_cmd(self) -> List[str]:
        return [self.docker_exe, "images"]

    def docker_containers_cmd(self, filters: List[str] = []) -> List[str]:
        if len(filters) > 0:
            fs: Iterator[str] = map(lambda x: "--filter {}".format(x), filters)
            flts: str = " ".join(fs)
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

    def docker_initialize_cmd(self) -> List[str]:
        return [self.docker_exe,                                # docker
                "build",                                        # build an image
                "-f={}".format(self.dockerfile),                # location of dockerfile
                "-t={}".format(self.docker_image_fullname()),   # name of the image
                "../"                                           # working directory for build
                ]
            
    def docker_run_container_cmd(self) -> List[str]:
        # docker run --security-opt seccomp=unconfined --name benchmark-calc -dit dbarowy/benchmark-calc:v1
        return [self.docker_exe,                                            # docker
                'run',                                                      # run an image
                '--security-opt seccomp=unconfined',                        # enable ptrace 
                '--name {}'.format(self.docker_container_name()),           # container name
                '-dit {}'.format(self.docker_image_fullname())              # image name
                ]

    def docker_exec_dodo_cmd(self, env: Dict[str,str] = {}) -> List[str]:
        # docker exec -e {environment=variables} benchmark-calc /benchmark/run.sh
        if len(env) > 0:
            env_strings: List[str] = []
            for key in env:
                env_strings += ["-e {}='{}'".format(key, env[key])]

            arr: List[str] = []
            arr += [self.docker_exe, "exec"]
            arr += env_strings                                                  # environment variables
            arr += ["{}".format(self.docker_container_name()),                  # the name of the running container
                    "{}".format(self.docker_dodo_runner)                        # the path to the program in the container
                    ]
            return arr
        else:
            return [self.docker_exe,                                            # docker
                    "exec",                                                     # run a program inside a container
                    "{}".format(self.docker_container_name()),                  # the name of the running container
                    "{}".format(self.docker_dodo_runner)                        # the path to the program in the container
                    ]

    def no_docker_exec_dodo_cmd(self) -> List[str]:
        return [self.no_docker_dodo_runner]

    def docker_exec_make_cmd(self, env: Dict[str,str] = {}) -> List[str]:
        # docker exec -e {environment=variables} benchmark-calc /benchmark/run.sh
        if len(env) > 0:
            env_strings: List[str] = []
            for key in env:
                env_strings += ["-e {}='{}'".format(key, env[key])]

            arr: List[str] = []
            arr += [self.docker_exe, "exec"]
            arr += env_strings                                                  # environment variables
            arr += ["{}".format(self.docker_container_name()),                  # the name of the running container
                    "{}".format(self.docker_make_runner)                        # the path to the program in the container
                    ]
            return arr
        else:
            return [self.docker_exe,                                            # docker
                    "exec",                                                     # run a program inside a container
                    "{}".format(self.docker_container_name()),                  # the name of the running container
                    "{}".format(self.docker_make_runner)                        # the path to the program in the container
                    ]

    def no_docker_exec_make_cmd(self) -> List[str]:
        return [self.no_docker_make_runner]

    def docker_stop_container_cmd(self) -> List[str]:
        return [self.docker_exe,                                            # docker
                'stop',                                                     # stop container
                '{}'.format(self.docker_container_name()),                  # container name
                ]

    def docker_rm_container_cmd(self) -> List[str]:
        return [self.docker_exe,                                            # docker
                'rm',                                                       # remove container
                '{}'.format(self.docker_container_name()),                  # container name
                ]

    def docker_rm_image_cmd(self) -> List[str]:
        return [self.docker_exe,                                            # docker
                'rmi',                                                      # remove image
                '{}'.format(conf.docker_image_fullname()),                  # image name
                ]

    # copy a file in a docker container to a local file
    def docker_cp_file_cmd(self, docker_file: str, local_file: str) -> List[str]:
        return [self.docker_exe,
                "cp",
                "{}:{}".format(self.docker_container_name(), docker_file),
                local_file
                ]

    # remove a file in a docker container
    def docker_rm_file_cmd(self, docker_file: str, ignore_failure: bool, recursive: bool) -> List[str]:
        flags: str = ""
        if ignore_failure or recursive:
            flags = "-"
            if ignore_failure:
                flags += "f"
            if recursive:
                flags += "r"
        return [self.docker_exe,
                "exec",                                                     # run a program inside a container
                "{}".format(self.docker_container_name()),                  # the name of the running container
                "rm",                                                       # the path to rm
                "{}".format(flags),                                         # flags for rm
                docker_file                                                 # path to file to be deleted
                ]

    # query running container for Docker stats
    def docker_stats_cmd(self) -> List[str]:
        return [self.docker_exe,
                "stats",                        # get container stats
                "--no-stream",                  # just return the latest value
                "--format={{.BlockIO}}",        # all we care about is block I/O
                conf.docker_container_name()    # for this container
        ]

    # initialize the no-Docker benchmark
    def no_docker_init_cmd(self) -> List[str]:
        return [self.no_docker_init_script,     # path to init script
                self.benchmark_root,            # location to initialize benchmark
                self.benchmark_name             # name of the benchmark
        ]

    # returns true if image already set up
    def image_is_initialized(self) -> bool:
        if self.no_docker:
            # if the path exists, good enough
            return os.path.isdir(self.no_docker_path)
        else:
            rc: int; rv: str
            rc, rv = run_command_capture(self.docker_images_cmd(), suppress_printing = False)
            if rc == 0:
                first: bool = True
                for line in rv.splitlines():
                    if first:
                        first = False
                        continue
                    rx: str = r"(?P<repository>[^\s]+)\s+(?P<tag>[^\s]+)\s+(?P<image_id>[0-9a-z]+)\s+(?P<created>.+)\s+(?P<size>[0-9.]+.B)"
                    p: Pattern[str] = re.compile(rx, re.IGNORECASE)
                    m: Optional[Match[str]] = p.search(line)
                    if m and (m.group("repository") == self.docker_image_name()) and (m.group("tag") == self.docker_image_version()):
                        return True
                return False
            else:
                print("ERROR: Unable to query docker ({}) for image data (return code: {}).".format(self.docker_exe, rc))
                sys.exit(1)

    # returns true if container is running
    def container_is_running(self) -> bool:
        if self.no_docker:
            # if the path exists, "the container is running"
            return os.path.isdir(self.no_docker_path)
        else:
            if not self.image_is_initialized():
                print("ERROR: Docker image '{}' is not initialized".format(self.docker_image_fullname()))
                sys.exit(1)
            rc: int; rv: str
            rc, rv = run_command_capture(self.docker_containers_cmd(["status=running"]))
            if rc == 0:
                first: bool = True
                for line in rv.splitlines():
                    rx: str = r"(?P<container_id>[^\s]+):(?P<image_id>[^\s]+):(?P<container_name>[^\s]+):(?P<status>.+)"
                    p: Pattern[str] = re.compile(rx, re.IGNORECASE)
                    m: Optional[Match[str]] = p.search(line)
                    if m and (m.group("container_name") == self.docker_container_name()):
                        return True
                return False
            else:
                print("ERROR: Unable to query docker ({}) for container data (return code: {}).".format(self.docker_exe, rc))
                sys.exit(1)

    # returns true if container is dead
    def container_is_dead(self) -> bool:
        if self.no_docker:
            # if the path exists, "the container is not dead"
            return not os.path.isdir(self.no_docker_path)
        else:
            if not self.image_is_initialized():
                print("ERROR: Docker image '{}' is not initialized".format(self.docker_image_fullname()))
                sys.exit(1)
            rc: int; rv: str
            rc, rv = run_command_capture(self.docker_containers_cmd(["status=dead", "status=exited"]))
            if rc == 0:
                first: bool = True
                for line in rv.splitlines():
                        rx: str = r"(?P<container_id>[^\s]+):(?P<image_id>[^\s]+):(?P<container_name>[^\s]+):(?P<status>.+)"
                        p: Pattern[str] = re.compile(rx, re.IGNORECASE)
                        m: Optional[Match[str]] = p.search(line)
                        if m:
                            status = m.group("status")
                            if m.group("container_name") == self.docker_container_name():
                                return True
                return False
            else:
                print("ERROR: Unable to query docker ({}) for container data (return code: {}).".format(self.docker_exe, rc))
                sys.exit(1)

    # initializes a docker image
    def initialize_docker_image(self) -> None:
        if self.no_docker:
            rc = run_command(self.no_docker_init_cmd())
            if (rc != 0):
                print("ERROR: Unable to initialize no-Docker benchmark.")
                sys.exit(1)
        else:
            rc = run_command(self.docker_initialize_cmd())
            if rc != 0:
                print("ERROR: Unable to create docker image.")
                sys.exit(1)

    # start docker image
    def start_container(self) -> None:
        if not self.no_docker:
            rc = run_command(self.docker_run_container_cmd())
            if rc != 0:
                print("ERROR: Unable to start docker container.")
                sys.exit(1)

    # run dodo benchmark
    # returns whatever return code was returned by the benchmark
    def benchmark_exec(self, tool: Tool) -> int:
        # setup environment
        my_env: Dict[str,str] = {
            "BENCHMARK_NAME": self.benchmark_name,
            "BENCHMARK_ROOT": self.benchmark_root,
            "TIME_CSV": self.time_data_csv,
            "DODO_EXE": self.dodo_exe
        }

        # choose appropriate benchmark command
        cmd: List[str]
        if self.no_docker:
            if tool == Tool.DODO:
                cmd = self.no_docker_exec_dodo_cmd()
            elif tool == Tool.MAKE:
                cmd = self.no_docker_exec_make_cmd()
            else:
                raise Exception("Unknown tool.")
        else:
            if tool == Tool.DODO:
                cmd = self.docker_exec_dodo_cmd(my_env)
            elif tool == Tool.MAKE:
                cmd = self.docker_exec_make_cmd(my_env)
            else:
                raise Exception("Unknown tool.")

        # sanity check if running in Docker mode
        if not self.no_docker and not self.container_is_running():
                print("ERROR: Cannot run benchmark without a running docker image.")
                sys.exit(1)

        # run benchmark script
        rc: int = run_command(cmd, my_env) if self.no_docker else run_command(cmd)
        if rc != 0:
            print("ERROR: Benchmark returned code {}.".format(rc))
        return rc

    # copies the given file from the running docker container
    # to a new tempfile and returns the name of that tempfile
    def copy_docker_file(self, file: str) -> str:
        t: str
        _, t = tempfile.mkstemp()
        args: List[str] = self.docker_cp_file_cmd(file, t)
        rc: int = run_command(args)
        if rc != 0:
            print("ERROR: Unable to copy file '{}' from docker.".format(file))
        return t

    # stop container and remove it
    # deletes the image too when rm_image is true
    # when ignore_failure is true, just keep chugging along even
    # if commands fail
    def benchmark_remove(self, no_docker: bool, rm_image: bool, ignore_failure: bool = False) -> int:
        if no_docker:
            # we just remove the folder at no_docker_path
            return rmdir(self.benchmark_root, ignore_errors = ignore_failure)
        else:
            rc: int
            rc, _ = run_command_capture(conf.docker_stop_container_cmd(), suppress_printing=ignore_failure)
            if rc != 0 and not ignore_failure:
                print("ERROR: Unable to stop container '{}'.".format(conf.docker_container_name()))
                # don't die, just return so that results can be
                # written out later
                return rc
            time.sleep(1)    # wait a little bit for the container to stop

            rc, _ = run_command_capture(conf.docker_rm_container_cmd(), suppress_printing=ignore_failure)
            if rc != 0 and not ignore_failure:
                print("ERROR: Unable to remove stopped container '{}'".format(conf.docker_container_name()))
                # don't die; see above
                return rc
            time.sleep(1)    # wait a little bit for the container to go away
            
            if rm_image:
                rc, _ = run_command_capture(conf.docker_rm_image_cmd(), suppress_printing=ignore_failure)
                if rc != 0 and not ignore_failure:
                    print("ERROR: Unable to remove image '{}'".format(conf.docker_image_fullname()))
                    # again, don't die
            time.sleep(1)    # wait a little bit for the image to go away
            return rc

    def docker_rm_file(self, path: str, ignore_failure: bool, recursive: bool) -> int:
        cmd: List[str] = conf.docker_rm_file_cmd(path, ignore_failure, recursive)
        rc: int
        rc, _ = run_command_capture(cmd, suppress_printing=ignore_failure)
        if rc != 0:
            print("ERROR: Unable to remove file '{}' in docker container '{}'.".format(path, conf.docker_container_name()))
        return rc

    def docker_stats(self) -> DockerStats:
        cmd: List[str] = conf.docker_stats_cmd()
        rc: int; rv: str
        rc, rv = run_command_capture(cmd)
        if rc != 0:
            print("ERROR: Unable to obtain docker stats for docker container '{}'.".format(conf.docker_container_name()))
            sys.exit(1)

        rx: str = r"(?P<input_count>[0-9]+(.[0-9]+)?)(?P<input_unit>B|kB|MB|GB) / (?P<output_count>[0-9]+(.[0-9]+)?)(?P<output_unit>B|kB|MB|GB)"
        p: Pattern[str] = re.compile(rx, re.IGNORECASE)
        line: str = rv.strip()
        m: Optional[Match[str]] = p.search(line)
        if m:
            input_count = m.group("input_count")
            input_unit = m.group("input_unit")
            output_count = m.group("output_count")
            output_unit = m.group("output_unit")
            input_bytes = DockerStats.toBytes(input_count, input_unit)
            output_bytes = DockerStats.toBytes(output_count, output_unit)
            ds: DockerStats = DockerStats(input_bytes, output_bytes)
            return ds
        raise Exception("Unable to parse Docker stats output:\n'{}'".format(line))

## FUNCTION DEFINITIONS

# read configuration
def init_configs(args: List[str]) -> Configs:
    parser = argparse.ArgumentParser(description="Runs a benchmark given at least one JSON "
                                                 "configuration file and a CSV for output.  If the "
                                                 "CSV already exists, this program appends output "
                                                 "to it. By default, this program will display the "
                                                 "configuration and then wait for the user to "
                                                 "confirm before running the given benchmarks.")
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
    parser.add_argument("--dodo",
                        help="measure clean build baseline using make",
                        action="store_true")   
    parser.add_argument("--dont-ask",
                        help="don't ask for user confirmation, just run",
                        action="store_true")
    parser.add_argument("--incr-none-make",
                        help="measure incremental rebuild time for no changes using make (implies --make)",
                        action="store_true")
    parser.add_argument("--incr-none-dodo",
                        help="measure incremental rebuild time for no changes using dodo (implies --dodo)",
                        action="store_true")
    parser.add_argument("--make",
                        help="measure clean build baseline using make",
                        action="store_true")
    parser.add_argument("--no-docker",
                        help="run outside of docker (assumes prerequisites are installed!)",
                        action="store_true")
    pargs = parser.parse_args()

    try:
        return Configs(pargs)
    except OSError:
        print("ERROR: Cannot read config file '" + pargs.config + "'")
        sys.exit(1)

# removes a file, and doesn't complain if it doesn't exist
def rm_silently(file: str) -> None:
    try:
        os.remove(file)
    except OSError:
        pass

# removes a directory, optional complaining
def rmdir(path: str, ignore_errors = False) -> int:
    # rmtree doesn't appear to return anything; simulate success/failure using try
    try:
        shutil.rmtree(path, ignore_errors)
    except Exception:
        print("ERROR: Unable to remove path '{}'.".format(path))
        return 1
    return 0

# runs a command, with optional updated environment
# variables, printing output as it runs.
# returns a return code.
def run_command(command: List[str], env: Dict[str,str] = {}) -> int:
    rc: int
    rc, _ = run_command_capture(command, env, suppress_printing=False)
    return rc

# runs a command, with optional updated environment
# variables, saving output to a string as it runs.
# returns a return code and the output string
def run_command_capture(command: List[str],
                        env: Dict[str, str] = {},
                        suppress_printing: bool = True) -> Tuple[int,str]:
    # mash args into a string, and use shell=True
    # because Python does not correctly process arguments
    # containing equals signs.
    args: str = " ".join(command)

    # obtain a copy of the current environment
    cur_env: Dict[str,str] = os.environ.copy()

    # override using supplied variables
    cur_env.update(env)

    # call the process, with modified environment
    process: Popen[bytes] = Popen(args, stdout=PIPE, shell=True, env=cur_env)
    cap: str = ""
    while True:
        if process.stdout:
            output: bytes = process.stdout.readline()
            if not output:
                break
            s: str = output.decode('utf-8')
            cap += s
            if not suppress_printing:
                print(s.strip())
    rc: Optional[int] = process.poll()
    if rc is not None:
        return rc, cap
    else:
        raise Exception("Cannot obtain return code for command '{}'.".format(command[0]))

# if csv does not exist, create file and write header;
# otherwise append
def csv_append(file: str, header: str, rows: List[str]) -> None:
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
def csv_read(file: str) -> CSV:
    with open(file, 'r') as fh:
        header: str = ""
        first: bool = True
        rows: List[str] = []

        for line in fh.readlines():
            if first:
                header = line.strip()
                first = False
            else:
                rows += [line.strip()]
        return (header, rows)

# reads two csv files and returns the result
# merged in the form of a (header, rows)
def merge_csvs(file1: str, file2: str) -> CSV:
    (h1, rs1) = csv_read(file1)
    (h2, rs2) = csv_read(file2)
    header = h1 + "," + h2
    assert len(rs1) == len(rs2)
    rows: List[str] = []
    for i, _ in enumerate(rs1):
        rows += [rs1[i] + "," + rs2[i]]
    return (header, rows)

# prepend the CSV with the given header and column data
def prepend_column(header: str, column: List[str], csv_header: str, csv_rows: List[str]) -> CSV:
    # add the new column to the header
    h2: str = "\"" + header + "\"," + csv_header

    # add the data to the rows
    assert len(column) == len(csv_rows)
    rows: List[str] = []
    for i, _ in enumerate(csv_rows):
        rows += ["\"" + column[i] + "\"," + csv_rows[i]]
    return h2, rows

# append the CSV with the given header and column data
def append_column(header: str, column: List[str], csv_header: str, csv_rows: List[str]) -> CSV:
    # add the new column to the header
    h2: str = csv_header + ",\"" + header + "\""

    # add the data to the rows
    assert len(column) == len(csv_rows)
    rows: List[str] = []
    for i, _ in enumerate(csv_rows):
        rows += [csv_rows[i] + ",\"" + column[i] + "\""]
    return h2, rows

# runs the configured benchmark, returning a CSV data structure
# (not a file) as output
def run_benchmark(conf: Config, tool: Tool, task: Task) -> CSV:

    d: str = "RUNNING BENCHMARK for tool '{}' and task '{}'\n".format(tool, task)
    open('/home/dbarowy/Documents/Code/dodo/debug.txt', 'a+').write(d)

    # run benchmark
    rc: int = conf.benchmark_exec(tool)

    header: str     # we don't know what the header is yet
    rows: List[str] # nor the rows

    # if there was no error, copy CSV data
    if rc == 0:
        dodo_time_tmpfile: str = conf.time_data_csv

        # if running outside Docker, stats are already in the right place
        if not conf.no_docker:
            # copy outputs to 'local' machine
            dodo_time_tmpfile = conf.copy_docker_file(conf.time_data_csv)

        # read csv and return as (header, rows)
        header, rows = csv_read(dodo_time_tmpfile)

    # get the number of rows
    nrows: int = len(rows)
    
    # record the return code-- is nonzero in case of error
    header, rows = prepend_column("return_code", [str(rc)] * nrows, header, rows)

    # build task
    header, rows = prepend_column("build_task", [str(task)] * nrows, header, rows)

    # add tool
    header, rows = prepend_column("tool", [str(tool)] * nrows, header, rows)

    # add benchmark name
    header, rows = prepend_column("benchmark_name", [conf.benchmark_name] * nrows, header, rows)

    # add docker mode
    header, rows = prepend_column("docker_mode", ["TRUE" if not conf.no_docker else "FALSE"] * nrows, header, rows)

    return header, rows

# run a suite of benchmarks for a given configuation
def run_suite(conf: Config, tool: Tool, rebuild: bool, needs_cleanup: bool) -> None:
    # stop and delete container if something ran before
    if needs_cleanup:
        rc: int = conf.benchmark_remove(no_docker = conf.no_docker, rm_image = False, ignore_failure = False)
        if (rc != 0):
            print("Unable to reset docker configuration to run '{}' benchmark.".format(conf.benchmark_name))
            sys.exit(1)

        # if this is no-Docker, then we need to put the benchmark folder back
        if conf.no_docker:
            conf.initialize_docker_image()

    # start docker image, if necessary
    if conf.container_is_dead():
        if not conf.no_docker:
            print("INFO: Docker container '{}' is dead.  Removing old container and restarting...".format(conf.docker_container_name()))
        conf.benchmark_remove(no_docker = conf.no_docker, rm_image = False)
        conf.start_container()
    elif not conf.container_is_running():
        if not conf.no_docker:
            print("INFO: Docker container '{}' is not running.  Starting...".format(conf.docker_container_name()))
        conf.start_container()
    else:
        if not conf.no_docker:
            print("INFO: Docker container '{}' is already running.  Skipping startup.".format(conf.docker_container_name()))

    # run benchmark and obtain CSV result
    header, rows = run_benchmark(conf, tool, Task.FULL)

    # write out results
    csv_append(conf.output_csv, header, rows)

    # rebuild with dodo?
    if rebuild:
        # run benchmark and obtain CSV result
        header, rows = run_benchmark(conf, tool, Task.REBUILD_NO_CHANGES)

        # write out results
        csv_append(conf.output_csv, header, rows)

## MAIN METHOD

# init config
c: Configs = init_configs(sys.argv)
print(c)

if not c.dont_ask:
    yn: str = input("DO YOU WANT TO CONTINUE? [Y/n] ")
    if yn != "" and yn != "Y" and yn != "y":
        sys.exit(0)

for conf in c.configs:
        needs_cleanup: bool = False

        # if the user asked us to start with a clean slate, do so
        if conf.cleanup_before:
            print("INFO: Pre-cleaning experiment setup...")
            conf.benchmark_remove(no_docker = conf.no_docker, rm_image = True, ignore_failure = True)

        # initialize docker container, if necessary
        if not conf.image_is_initialized():
            if conf.no_docker:
                print("INFO: No-docker benchmark '{}' is not initialized.  Initializing...".format(conf.benchmark_name))
            else:
                print("INFO: Docker image '{}' is not initialized.  Initializing...".format(conf.docker_image_fullname()))
            conf.initialize_docker_image()
        else:
            print("INFO: Docker image '{}' is already initialized.  Skipping initialization.".format(conf.docker_image_fullname()))

        # build with dodo?
        if conf.dodo:
            run_suite(conf, Tool.DODO, conf.incr_none_dodo, needs_cleanup)
            needs_cleanup = True

        # build with make?
        if conf.make:
            run_suite(conf, Tool.MAKE, conf.incr_none_make, needs_cleanup)

        # tear down docker containers & optionally delete image
        if conf.cleanup_after:
            conf.benchmark_remove(no_docker = conf.no_docker, rm_image = True)

        # tell the user that we are finished
        print("INFO: DONE: " + conf.benchmark_name)
        print("INFO: RESULTS APPENDED TO: " + conf.output_csv)
