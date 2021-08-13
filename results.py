#!/usr/bin/env python3

import os
from os import path
import statistics

RKR_DIR = path.abspath(path.dirname(__file__))
BENCH_DIR = path.join(RKR_DIR, 'benchmarks')

summary = open('results/summary.txt', 'w')

def gather_times(name):
  csv = open('results/{}.csv'.format(name), 'w')
  print('program,default_time,rkr_time,overhead,difference', file=csv)

  overheads = []
  diffs = []

  # Loop over the benchmark directories to gather build time data
  for bench in os.listdir(BENCH_DIR):
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
        overhead = rkr_time / default_time
        diff = rkr_time - default_time
        overheads.append(overhead)
        diffs.append(diff)
        print('{},{},{},{}'.format(bench, default_time, rkr_time, overhead, diff), file=csv)
  
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

gather_times('full-build')
gather_times('nop-build')

summary.close()

summary = open('results/summary.txt', 'r')
for line in summary:
  print(line, end='')
