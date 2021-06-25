# Riker Benchmarks HOWTO

This repository contains a number of benchmarks to observe Riker's performance on real-world builds.  To aid in reproducibility and in performance measurement, benchmarks are run in Docker containers.  Although there is a small performance penalty being paid by running in a container, all benchmarks are equally affected, and Docker is now a common deployment target, so we think the disadvantages of such a configuration are small.

We have only tested these benchmarks on Linux.  Although Docker is available on other platforms, we provide no guarantees about these benchmarks on those other platforms.

## Required software:

You will need to have the following software installed.  The version we used for our tests is in parentheses.  Note that we performed all tests using Ubuntu 20.04.

* Python 3 (3.8.6)
* Docker (9.03.13, build 4484c46)

## Running all benchmarks

For your convenience, a script called `run-all.sh` is available in the `benchmarks` directory.  It will run all of the current benchmarks, outputting to the CSV you specify.  For example,

```
$ ./run-all.sh out.csv
```

will run all of the benchmarks, appending data for each run to `out.csv`.

## Running benchmarks individually

An individual benchmark can be run by running the `run.py` program, found in the `benchmarks` directory.  Online help is available by running:

```
$ ./run.py --help
usage: run.py [-h] [--cleanup-before] [--cleanup-after] [--dont-ask] [--incr-none] output config [config ...]

Runs a benchmark given at least one JSON configuration file and a CSV for output. If the CSV already exists, this program appends output to it. If more
than one config is supplied, by default, this program will wait for the user to confirm that they actually want to run all of given benchmarks.

positional arguments:
  output            path to a CSV for benchmark output
  config            path to a JSON benchmark configuration file

optional arguments:
  -h, --help        show this help message and exit
  --cleanup-before  remove conflicting container and image before running
  --cleanup-after   shut down and remove Docker image after running
  --dont-ask        don't ask for user confirmation, just run
  --incr-none       measure incremental rebuild time for no changes
```

For example, to run the `calc` benchmark, outputting statistics to a file called `out.csv`, we would run:

```
$ ./run.py out.csv calc/benchmark.json
```

If `out.csv` does not exist, `run.py` will create it, with headers, for easy analysis.  If it already exists, it will append data-only rows.  This setup makes it easy to run several benchmarks, storing benchmark statistics in the same file.

## Adding a benchmark

Each benchmark is collected in a subdirectory in this directory, and each is defined by a `Dockerfile` and a `benchmark.json` file.  The `Dockerfile` specifies all of the setup actions (e.g., running `apt install`) for a given benchmark, including copying any helper scripts needed inside the container.  The `benchmark.json` file tells the `run.py` script where to find the benchmark and its helper scripts.

#### Helper script

Each benchmark should have the following helper script.  You should generally not need to change this script from benchmark to benchmark, as each benchmark's `Rikerfile` describes precisely what to do during a build.

* A _runner script_, which starts a Riker build wrapped in a call to `time`.  This script should log the output of `time` to a CSV with the header `"\"wall_s\",\"system_s\",\"user_s\",\"pct_cpu\",\"avg_rss\""` and using the following format string: `"\"%e\",\"%S\",\"%U\",\"%P\",\"%t\""`

#### `benchmark.json`

This file should have the following fields set:

* `name`: The name of the benchmark.
* `benchmark_root`: The folder the benchmark is stored in, conventionally, `/benchmark`.
* `tmp_csv`: The location that Riker will write its `--stats` output.
* `time_data_csv`: The location of the CSV written to by the `time` command called in `run.sh` (see above).
* `docker_runner`: The location of the runner script (see above).
* `image_version`: Just leave this as `1` unless you have a reason to change it (Docker needs a version for containers).

Here is a sample `benchmark.json` file for `calc`.  Refer to each benchmark for more examples.  Note that the order of the fields does not matter.

```
{
    "name": "calc",
    "benchmark_root": "/benchmark",
    "tmp_csv": "/benchmark/tmp.csv",
    "time_data_csv": "/benchmark/time.csv",
    "docker_runner": "/benchmark/run.sh",
    "image_version": "1"
}
```

## Notes

This program creates a Docker image using the following command:

```
$ docker build -f <Dockerfile> -t <username>/benchmark-<benchmark name>:v1 ../
```

It starts a container using:

```
$ docker run --security-opt seccomp=unconfined --name benchmark-<benchmark name> -dit <username>/benchmark-<benchmark name>:v1
```

It executes a benchmark using the runner script with:

```
$ docker exec benchmark-<benchmark name> <benchmark root>/<runner script>
```