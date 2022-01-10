#!/usr/bin/env python3

import json
import os
from os import path
import shutil
import sys
import time

RKR_DIR = path.abspath(path.dirname(__file__))
BENCH_DIR = path.join(RKR_DIR, 'benchmarks')
BENCHMARKS = {}
DEFAULT_REPS = 5
COMMIT_COUNT = 100

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

def checkout_rev(name, rev):
  print('  Checking out {} revision {}'.format(name, rev))

  bench_path = path.join(BENCH_DIR, name)
  checkout_path = path.join(bench_path, 'checkout')
  rc = os.system('cd {}; git checkout -f -q {} > /dev/null'.format(checkout_path, rev))
  if rc != 0:
    raise Exception('Git checkout of revision {} for {} failed'.format(rev, name))

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
      checkout_rev(name, rev)
    else:
      print('  WARNING: benchmark {} uses git but does not specify a commit. Using the repository as-is'.format(name))
    
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

def copy_files(name, build_tool):
  print('  Copying files to {} for {} build'.format(name, build_tool))

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

def setup(name, build_tool):
  print('  Setting up {} for {} build'.format(name, build_tool))

  bench_path = path.join(BENCH_DIR, name)
  checkout_path = path.join(bench_path, 'checkout')
  
  # Are there setup commands to run?
  if 'setup' in BENCHMARKS[name][build_tool]:
    for cmd in BENCHMARKS[name][build_tool]['setup']:
      print('    Running {}'.format(cmd))
      rc = os.system('cd {}; {} 2> /dev/null 1> /dev/null'.format(checkout_path, cmd))
      if rc != 0:
        raise Exception('Setup command {} in benchmark {} failed'.format(cmd, name))

def full_build(name, build_tool):
  bench_path = path.join(BENCH_DIR, name)
  checkout_path = path.join(bench_path, 'checkout')

  print('Full {} build of {}'.format(build_tool, name))
  checkout(name)

  reps = DEFAULT_REPS
  if 'reps' in BENCHMARKS[name]:
    reps = int(BENCHMARKS[name]['reps'])

  full_time = open(path.join(bench_path, 'full-build-{}.csv'.format(build_tool)), 'w')
  nop_time = open(path.join(bench_path, 'nop-build-{}.csv'.format(build_tool)), 'w')

  for i in range(0, reps):
    setup(name, build_tool)
    copy_files(name, build_tool)
    print('  Running build {}'.format(i+1))

    build_cmd = BENCHMARKS[name][build_tool]['build']

    start_time = time.perf_counter()
    rc = os.system('cd {}; {} 2> /dev/null 1> /dev/null'.format(checkout_path, build_cmd))
    end_time = time.perf_counter()

    print('{:.4f}'.format(end_time - start_time), file=full_time)
    print('    Finished in {:.2f}s with exit code {}'.format(end_time - start_time, rc))

    print('  Running no-op build {}'.format(i+1))
    start_time = time.perf_counter()
    rc = os.system('cd {}; {} 2> /dev/null 1> /dev/null'.format(checkout_path, build_cmd))
    end_time = time.perf_counter()
    
    print('{:.4f}'.format(end_time - start_time), file=nop_time)
    print('    Finished in {:.2f}s with exit code {}'.format(end_time - start_time, rc))

# Count lines in a file (a list of commands) but exclude lines with known prefixes
def count_lines(filepath, filter=[]):
  f = open(filepath, 'r')
  count = 0
  for line in f:
    counted = True

    # Make sure the line isn't empty
    if len(line.strip()) == 0:
      counted = False
    
    # Check to see if the line matches any of the filtered prefixes
    for pattern in filter:
      if line.startswith(pattern):
        counted = False

      parts = line.split(' ')
      if path.basename(parts[0]) in filter:
        counted = False
    
    if counted:
      count += 1
  return count

def rkr_case_study(name):
  bench_path = path.join(BENCH_DIR, name)
  checkout_path = path.join(bench_path, 'checkout')

  # Set up the repository for our first build
  end_commit = BENCHMARKS[name]['commit']
  commit = '{}~{}'.format(end_commit, COMMIT_COUNT)
  checkout_rev(name, commit)

  # Save the old rkr-commands directory and create a new one
  rkr_commands = path.join(bench_path, 'rkr-commands')
  old_rkr_commands = path.join(bench_path, '.old-rkr-commands')
  if path.isdir(rkr_commands):
    if path.isdir(old_rkr_commands):
      shutil.rmtree(old_rkr_commands)
    os.rename(rkr_commands, old_rkr_commands)
  os.mkdir(rkr_commands)

  rkr_csv = path.join(bench_path, 'case-study-rkr.csv')
  old_rkr_csv = path.join(bench_path, '.old-case-study-rkr.csv')
  # Save the old data file if it exists
  if os.path.isfile(rkr_csv):
    os.rename(rkr_csv, old_rkr_csv)
  
  # Open a csv file to write data to
  csv = open(rkr_csv, 'w')
  print('build,commands,runtime,db_size,cache_size,cache_count', file=csv)

  for i in range(0, COMMIT_COUNT + 1):
    # Check out the next revision
    commit_distance = COMMIT_COUNT - i
    commit = '{}~{}'.format(end_commit, commit_distance)
    checkout_rev(name, commit)
    copy_files(name, 'rkr')

    # Run the incremental build
    print('  Running incremental build at commit {}'.format(i))
    cmds_path = os.path.join(rkr_commands, '{:0>3}'.format(i))

    start_time = time.perf_counter()
    rc = os.system('cd {}; rkr --show-full -o {} 2> /dev/null 1> /dev/null'.format(checkout_path, cmds_path))
    runtime = time.perf_counter() - start_time
    print('    Finished in {:.2f}s with exit code {}'.format(runtime, rc))
    if rc != 0:
      raise Exception('Build failed')

    # Get the size of the riker database
    db_size = os.path.getsize('{}/.rkr/db'.format(checkout_path))

    # Compute the size of the riker cache
    cache_size = 0
    cache_count = 0
    for (dirname, subdirs, files) in os.walk('{}/.rkr/cache'.format(checkout_path)):
      for f in files:
        cache_count += 1
        cache_size += os.path.getsize(os.path.join(dirname, f))

    commands = count_lines(cmds_path)
    print('{},{},{},{},{},{}'.format(i, commands, runtime, db_size, cache_size, cache_count), file=csv)

def default_case_study(name):
  bench_path = path.join(BENCH_DIR, name)
  checkout_path = path.join(bench_path, 'checkout')

  cmd_filter = []
  if 'filter' in BENCHMARKS[name]['default']:
    cmd_filter = BENCHMARKS[name]['default']

  # Set up the repository for our first build
  end_commit = BENCHMARKS[name]['commit']
  commit = '{}~{}'.format(end_commit, COMMIT_COUNT)
  checkout_rev(name, commit)

  # Save the old default-commands directory and create a new one
  default_commands = path.join(bench_path, 'default-commands')
  old_default_commands = path.join(bench_path, '.old-default-commands')
  if path.isdir(default_commands):
    if path.isdir(old_default_commands):
      shutil.rmtree(old_default_commands)
    os.rename(default_commands, old_default_commands)
  os.mkdir(default_commands)

  audit_times = []
  command_counts = []
  filtered_command_counts = []

  for i in range(0, COMMIT_COUNT + 1):
    # Check out the next revision
    commit_distance = COMMIT_COUNT - i
    commit = '{}~{}'.format(end_commit, commit_distance)
    checkout_rev(name, commit)
    copy_files(name, 'default')

    # Run the incremental build
    print('  Running audit build at commit {}'.format(commit))
    cmds_path = path.join(default_commands, '{:0>3}'.format(i))

    start_time = time.perf_counter()
    rc = os.system('cd {}; rkr audit -o {} 2> /dev/null 1> /dev/null'.format(checkout_path, cmds_path))
    audit_time = time.perf_counter() - start_time
    print('    Finished in {:.2f}s with exit code {}'.format(audit_time, rc))
    if rc != 0:
      raise Exception('audit build failed')
    
    commands = count_lines(cmds_path)
    filtered_commands = count_lines(cmds_path, cmd_filter)

    audit_times.append(audit_time)
    command_counts.append(commands)
    filtered_command_counts.append(filtered_commands)
  
  # Reverse the lists of results from the first phase so we can pop each item
  audit_times.reverse()
  command_counts.reverse()
  filtered_command_counts.reverse()

  default_csv = path.join(bench_path, 'case-study-default.csv')
  old_default_csv = path.join(bench_path, '.old-case-study-default.csv')
  # Save the old data file if it exists
  if path.isfile(default_csv):
    os.rename(default_csv, old_default_csv)

  # Open a csv file to write data to
  csv = open(default_csv, 'w')
  print('build,commands,filtered_commands,runtime,audit_runtime', file=csv)

  for i in range(0, COMMIT_COUNT + 1):
    # Check out the next revision
    commit_distance = COMMIT_COUNT - i
    commit = '{}~{}'.format(end_commit, commit_distance)
    checkout_rev(name, commit)
    copy_files(name, 'default')

    # Run the incremental build
    print('  Running incremental build at commit {}'.format(i))
    cmds_path = os.path.join(default_commands, '{:0>3}'.format(i))

    build_cmd = BENCHMARKS[name]['default']['build']

    start_time = time.perf_counter()
    rc = os.system('cd {}; {} 2> /dev/null 1> /dev/null'.format(checkout_path, build_cmd, cmds_path))
    runtime = time.perf_counter() - start_time
    print('    Finished in {:.2f}s with exit code {}'.format(runtime, rc))
    if rc != 0:
      raise Exception('Build failed')

    commands = command_counts.pop()
    filtered_commands = filtered_command_counts.pop()
    audit_time = audit_times.pop()
    print('{},{},{},{},{}'.format(i, commands, filtered_commands, runtime, audit_time), file=csv)

def case_study(name, build_tool):
  print('Case study of {} with {}'.format(name, build_tool))
  checkout(name)
  setup(name, build_tool)
  if build_tool == 'rkr':
    rkr_case_study(name)
  else:
    default_case_study(name)

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
  print('  rattle      Build with rattle')
  print('  all         Run with all build tools')
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
  if sys.argv[2] not in ['default', 'rkr', 'rattle', 'all']:
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
          
      # Run the rattle build if requested
      if build_tool == 'rattle' or build_tool == 'all':
        try:
          full_build(bench, 'rattle')
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
          
      # Run the rkr build if requested
      if build_tool == 'rattle' or build_tool == 'all':
        try:
          case_study(bench, 'rattle')
        except Exception as e:
          print(e)
