# Riker Benchmarks HOWTO

This repository contains a number of benchmarks to observe Riker's performance on real-world builds.  To aid in reproducibility and in performance measurement, benchmarks are run in Docker containers.  Although there is a small performance penalty being paid by running in a container, all benchmarks are equally affected, and Docker is now a common deployment target, so we think the disadvantages of such a configuration are small.

We have only tested these benchmarks on Linux.  Although Docker is available on other platforms, we provide no guarantees about these benchmarks on those other platforms.

## Required software:

You will need to have the following software installed.  The version we used for our tests is in parentheses.  Note that we performed all tests using Ubuntu 20.04.

* Python 3 (3.8.6)
* Docker (9.03.13, build 4484c46)

## Running the benchmarks

A benchmark can be run by running the `run.py` program on your machine.  Online help is available by running:

```
$ ./run.py --help
usage: run.py [-h] config output

positional arguments:
  config      path to a JSON benchmark configuration file
  output      path to a CSV for benchmark output

optional arguments:
  -h, --help  show this help message and exit
```

For example, to run the `calc` benchmark, outputting statistics to a file called `out.csv`, we would run:

```
$ ./run.py calc/benchmark.json out.csv
```

If `out.csv` does not exist, `run.py` will create it, with headers, for easy analysis.  If it already exists, it will append data-only rows.  This setup makes it easy to run several benchmarks, storing benchmark statistics in the same file.

## Adding a benchmark

Each benchmark is collected in a subdirectory in this directory, and each is defined by a `Dockerfile` and a `benchmark.json` file.  The `Dockerfile` specifies all of the setup actions (e.g., running `apt install`) for a given benchmark, including copying any helper scripts needed inside the container.  The `benchmark.json` file tells the `run.py` script where to find the benchmark and its helper scripts.

#### Helper script

Each benchmark should have the following helper script.  You should generally not need to change this script from benchmark to benchmark, as each benchmark's `Dodofile` describes precisely what to do during a build.

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