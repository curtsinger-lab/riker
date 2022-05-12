#!/usr/bin/env python3

import os
from os import path
import statistics

RKR_DIR = path.abspath(path.join(path.dirname(__file__), '..'))
BENCH_DIR = path.join(RKR_DIR, 'benchmarks')

# Skip the xz-clang build. This was just an experiment to compare tracing overhead between gcc and clang (clang is much faster with rkr)
SKIP = ['xz-clang']

summary = open(path.join(RKR_DIR, 'results', 'summary.txt'), 'w')

def gather_times(name):
  csv = open(path.join(RKR_DIR, 'results', '{}.csv'.format(name)), 'w')
  print('program,default_time,rkr_time,overhead,difference', file=csv)

  overheads = []
  diffs = []

  # Loop over the benchmark directories to gather build time data
  for bench in os.listdir(BENCH_DIR):
    if bench in SKIP:
      continue

    # Does the benchmark have both results files?
    default_time_path = path.join(BENCH_DIR, bench, '{}-default.csv'.format(name))
    rkr_time_path = path.join(BENCH_DIR, bench, '{}-rkr.csv'.format(name))
    if path.isfile(default_time_path) and path.isfile(rkr_time_path):
      default_times = []
      f = open(default_time_path, 'r')
      for line in f:
        try:
          time = float(line)
          default_times.append(time)
        except Exception:
          pass
      
      rkr_times = []
      f = open(rkr_time_path, 'r')
      for line in f:
        try:
          time = float(line)
          rkr_times.append(time)
        except Exception:
          pass
      
      if len(default_times) > 0 and len(rkr_times) > 0:
        default_time = statistics.median(default_times)
        rkr_time = statistics.median(rkr_times)
        overhead = rkr_time / default_time - 1.0
        diff = rkr_time - default_time
        overheads.append(overhead)
        diffs.append(diff)
        print('{},{},{},{},{}'.format(bench, default_time, rkr_time, overhead, diff), file=csv)
  
  print('Overhead for {}'.format(name), file=summary)
  print('  Max: {:.3f}'.format(max(overheads)), file=summary)
  print('  Min: {:.3f}'.format(min(overheads)), file=summary)
  print('  Average: {:.3f}'.format(statistics.mean(overheads)), file=summary)
  print('  Median: {:.3f}'.format(statistics.median(overheads)), file=summary)
  print(file=summary)
  print('Absolute Difference in runtime for {}'.format(name), file=summary)
  print('  Max: {:.3f}'.format(max(diffs)), file=summary)
  print('  Min: {:.3f}'.format(min(diffs)), file=summary)
  print('  Average: {:.3f}'.format(statistics.mean(diffs)), file=summary)
  print('  Median: {:.3f}'.format(statistics.median(diffs)), file=summary)
  print(file=summary)

def case_study_savings():
  csv = open(path.join(RKR_DIR, 'results', 'case-study.csv'), 'w')
  print('program,build_tool,full_build,incremental_build,savings', file=csv)

  wait_times = []

  # Loop over the benchmark directories to gather build time data
  for bench in ['vim', 'redis', 'sqlite', 'xz', 'memcached', 'riker']:
    # Does the benchmark have both results files?
    default_path = path.join(BENCH_DIR, bench, 'case-study-default.csv')
    rkr_path = path.join(BENCH_DIR, bench, 'case-study-rkr.csv')
    if path.isfile(default_path) and path.isfile(rkr_path):
      # Read data for the default build
      default = open(default_path, 'r')
      default_lines = []
      for line in default:
        default_lines.append(line)
      
      # Strip off the headers
      default_lines = default_lines[1:]

      # Read the full build time
      (_, commands, filtered_commands, runtime, _) = default_lines[0].split(',')
      default_full_time = float(runtime)

      default_times = []

      default_total_full_time = 0.0
      default_total_incremental_time = 0.0

      # Read incremental build times
      for line in default_lines[1:]:
        try:
          (_, commands, filtered_commands, runtime, _) = line.split(',')
          default_total_incremental_time += float(runtime)
          default_total_full_time += default_full_time
          default_times.append(float(runtime))
        except Exception:
          pass
      
      if default_total_full_time > 0 and default_total_incremental_time > 0:
        print('{},Default,{},{},{}'.format(bench, default_total_full_time, default_total_incremental_time, 1 - (default_total_incremental_time / default_total_full_time)), file=csv)
    
      # Read data for the rkr build
      rkr = open(rkr_path, 'r')
      rkr_lines = []
      for line in rkr:
        rkr_lines.append(line)
      
      # Strip off the headers
      rkr_lines = rkr_lines[1:]

      # Read the full build time
      (_, commands, runtime, db_size, cache_size, cache_count) = rkr_lines[0].split(',')
      rkr_full_time = float(runtime)

      rkr_times = []
      rkr_total_full_time = 0.0
      rkr_total_incremental_time = 0.0

      # Read incremental build times
      for line in rkr_lines[1:]:
        try:
          (_, commands, runtime, db_size, cache_size, cache_count) = line.split(',')
          rkr_total_incremental_time += float(runtime)
          rkr_total_full_time += default_full_time
          rkr_times.append(float(runtime))
        except Exception:
          pass
      
      if default_total_full_time > 0 and rkr_total_incremental_time > 0:
        print('{},Riker,{},{},{}'.format(bench, default_total_full_time, rkr_total_incremental_time, 1 - (rkr_total_incremental_time / rkr_total_full_time)), file=csv)

      if len(default_times) == len(rkr_times):
        for (d, r) in zip(default_times, rkr_times):
          wait_times.append(r - d)
  
  print('Incremental Build Extra Wait Time', file=summary)
  print('  Max: {:.3f}'.format(max(wait_times)), file=summary)
  print('  Min: {:.3f}'.format(min(wait_times)), file=summary)
  print('  Mean: {:.3f}'.format(statistics.mean(wait_times)), file=summary)
  print('  Median: {:.3f}'.format(statistics.median(wait_times)), file=summary)

  wait_times.sort()
  index = int(float(len(wait_times))*.95)
  print('  95th: {:.3f}'.format(wait_times[index]), file=summary)

gather_times('full-build')
gather_times('nop-build')

case_study_savings()

summary.close()

summary = open(path.join(RKR_DIR, 'results', 'summary.txt'), 'r')
for line in summary:
  print(line, end='')
