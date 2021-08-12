#!/usr/bin/env python3

import json
import os
from os import path
import sys
import time

RKR_DIR = path.abspath(path.dirname(__file__))
BENCH_DIR = path.join(RKR_DIR, 'benchmarks')
BENCHMARKS = {}
REPS = 1

# Get information about all of the available benchmarks
for entry in os.listdir(BENCH_DIR):
  # Does the config file exist?
  config_path = path.join(BENCH_DIR, entry, 'config.json')
  if path.isfile(config_path):
    try:
      BENCHMARKS[entry] = json.load(open(config_path))
      if 'experiments' not in BENCHMARKS[entry]:
        del BENCHMARKS[entry]
      else:
        BENCHMARKS[entry]['experiments'].sort()
    except Exception as e:
      print('Failed to load config for benchmark {}: {}'.format(entry, e))

# Prepare a sorted list of benchmark names
BENCHMARK_NAMES = list(BENCHMARKS.keys())
BENCHMARK_NAMES.sort()

# Add the release version of rkr to the path
os.environ['PATH'] = path.join(path.abspath(RKR_DIR), 'release', 'bin') + ':' + os.environ['PATH']

def checkout(name):
  # Check to see if the benchmark is checked out
  bench_path = path.join(BENCH_DIR, name)
  checkout_path = path.join(bench_path, 'checkout')

  # How is the benchmark code managed? Options are git, or custom checkout commands
  if 'git' in BENCHMARKS[name]:
    # Do we need to clone the git repository?
    if not path.isdir(checkout_path):
      print('  Cloning benchmark {} from {}'.format(name, BENCHMARKS[name]['git']))
      rc = os.system('git clone -q {} {} > /dev/null'.format(BENCHMARKS[name]['git'], checkout_path))
      if rc != 0:
        raise Exception('Git clone of {} failed'.format(name))
    
    # Was a specific commit requested?
    if 'commit' in BENCHMARKS[name]:
      rev = BENCHMARKS[name]['commit']
      rc = os.system('cd {}; git checkout -f -q {} > /dev/null'.format(checkout_path, rev))
      if rc != 0:
        raise Exception('Git checkout of revision {} for {} failed'.format(rev, name))
    else:
      print('  WARNING: benchmark {} uses git but does not specify a commit. Using the repository as-is')
    
    # Reset the repository just to be safe
    rc = os.system('cd {}; git reset --hard > /dev/null'.format(checkout_path))
    if rc != 0:
      raise Exception('Git reset for benchmark {} failed'.format(name))
  
  elif 'checkout' in BENCHMARKS[name]:
    # Do we need a copy of the code?
    if not path.isdir(checkout_path):
      print('  Running custom checkout commands for benchmark {}'.format(name))
      for cmd in BENCHMARKS[name]['checkout']:
        rc = os.system('cd {}; {} > /dev/null'.format(bench_path, cmd))
        if rc != 0:
          raise Exception('Custom checkout of {} failed'.format(name))
      if not path.isdir(checkout_path):
        raise Exception('Custom checkout of {} did not create path {}'.format(name, checkout_path))

  else:
    raise Exception('No method found to checkout a copy of {}'.format(name))


def setup(name, build_tool):
  print('  Setting up {} for {} build'.format(name, build_tool))

  bench_path = path.join(BENCH_DIR, name)
  checkout_path = path.join(bench_path, 'checkout')

  # Are there any files to copy in?
  if 'copy' in BENCHMARKS[name][build_tool]:
    for (src, dest) in BENCHMARKS[name][build_tool]['copy'].items():
      real_src = path.join(bench_path, src)
      real_dest = path.join(checkout_path, dest)
      print('    Copying {} to {}'.format(path.relpath(real_src), path.relpath(real_dest)))
      rc = os.system('cp -R {} {}'.format(real_src, real_dest))
      if rc != 0:
        raise Exception('Copying {} for benchmark {} failed'.format(src, name))
  
  # Are there setup commands to run?
  if 'setup' in BENCHMARKS[name][build_tool]:
    for cmd in BENCHMARKS[name][build_tool]['setup']:
      print('    Running {}'.format(cmd))
      rc = os.system('cd {}; {} 2>&1 > /dev/null'.format(checkout_path, cmd))
      if rc != 0:
        raise Exception('Setup command {} in benchmark {} failed'.format(cmd, name))

def full_build(name, build_tool):
  bench_path = path.join(BENCH_DIR, name)
  checkout_path = path.join(bench_path, 'checkout')

  print('Full {} build of {}'.format(build_tool, name))
  checkout(name)

  for i in range(0, REPS):
    setup(name, build_tool)
    print('  Running build {}'.format(i+1))

    build_cmd = BENCHMARKS[name][build_tool]['build']
    log_path = path.join(bench_path, 'log-{}'.format(build_tool))

    start_time = time.perf_counter()
    rc = os.system('cd {}; {} 2>&1 > {}'.format(checkout_path, build_cmd, path.join(bench_path, log_path)))
    end_time = time.perf_counter()

    print('    Finished in {:.2f}s with exit code {}'.format(end_time - start_time, rc))

def case_study(name, build_tool):
  print('Case study of {} with {}'.format(name, build_tool))
  checkout(name)
  setup(name, build_tool)
  print('  Running case study')

def show_usage():
  print('Usage: {} <experiment> <build tool> <benchmark>...'.format(sys.argv[0]))
  print()
  print('Experiments:')
  print('  case-study  Record time and command counts for incremental builds')
  print('  full-build  Record the time to complete a full build')
  print('  all         Run both experiments')
  print()
  print('Build Tools:')
  print('  default     Use each benchmark\'s default build system')
  print('  rkr         Build with riker')
  print('  all         Run with both build tools')
  print()
  print('Benchmarks:')
  for bench in BENCHMARK_NAMES:
    exp = BENCHMARKS[bench]['experiments']
    print('  {: <10}  Supports {}'.format(bench, ' and '.join(exp)))
  print('  all         Run all benchmarks')

if __name__ == '__main__':
  if len(sys.argv) < 4:
    show_usage()
    exit(1)
  
  # Validate and unpack the experiment argument
  if sys.argv[1] not in ['case-study', 'full-build', 'all']:
    print('Invalid experiment name "{}"'.format(sys.argv[1]))
    show_usage()
    exit(1)
  else:
    experiment = sys.argv[1]
  
  # Validate and unpack the build tool argument
  if sys.argv[2] not in ['default', 'rkr', 'all']:
    show_usage()
    exit(1)
  else:
    build_tool = sys.argv[2]
  
  # Validate and unpack the benchmark arguments
  benchmarks = []
  if len(sys.argv) == 4 and sys.argv[3] == 'all':
    benchmarks = BENCHMARK_NAMES
  else:
    for arg in sys.argv[3:]:
      if arg not in BENCHMARK_NAMES:
        print('Invalid benchmark name "{}"'.format(arg))
        show_usage()
        exit(1)
      if experiment != 'all' and experiment not in BENCHMARKS[arg]['experiments']:
        print('Warning: benchmark {} does not support the {} experiment'.format(arg, experiment))
      benchmarks.append(arg)
  
  # Make sure there's at least one benchmark to run
  if len(benchmarks) == 0:
    print('No benchmarks are available to run in the requested configuration')
    show_usage()
    exit(1)

  # If rkr is going to be used make sure we have an updated release build
  if build_tool == 'rkr' or build_tool == 'all':
    print('Updating rkr release build')
    rc = os.system('cd {}; make release 2>&1 > /dev/null'.format(RKR_DIR))
    if rc != 0:
      print('Riker release build failed!')
      exit(1)
  
  # Run full-build experiments
  if experiment == 'full-build' or experiment == 'all':
    # Loop over benchmarks
    for bench in benchmarks:
      # If the benchmark doesn't support the full-build experiment, skip it
      if 'full-build' not in BENCHMARKS[bench]['experiments']:
        print('Full-build experiment is not available for {}'.format(bench))
        continue
      
      # Run the default build if requested
      if build_tool == 'default' or build_tool == 'all':
        try:
          full_build(bench, 'default')
        except Exception as e:
          print(e)
      
      # Run the rkr build if requested
      if build_tool == 'rkr' or build_tool == 'all':
        try:
          full_build(bench, 'rkr')
        except Exception as e:
          print(e)
  
  # Run case-study experiments
  if experiment == 'case-study' or experiment == 'all':
    # Loop over benchmarks
    for bench in benchmarks:
      # If the benchmark doesn't support the full-build experiment, skip it
      if 'case-study' not in BENCHMARKS[bench]['experiments']:
        print('Case study experiment is not available for {}'.format(bench))
        continue
      
      # Run the default build if requested
      if build_tool == 'default' or build_tool == 'all':
        try:
          case_study(bench, 'default')
        except Exception as e:
          print(e)
      
      # Run the rkr build if requested
      if build_tool == 'rkr' or build_tool == 'all':
        try:
          case_study(bench, 'rkr')
        except Exception as e:
          print(e)
