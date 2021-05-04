#!/usr/bin/env python3

import sys
import bench

REPO = 'https://github.com/xz-mirror/xz'
END_COMMIT = '2327a461e1afce862c22269b80d3517801103c1b'
COMMIT_COUNT = 100

# Copy these files over to the checkout directory before the rkr experiment
RKR_COPY_FILES = {'files/Rikerfile': 'Rikerfile'}

# Run these commands in the checkout directory before the make experiment
MAKE_SETUP_CMDS = [
  'cmake -G "Unix Makefiles" . > /dev/null',
  'make clean'
]

# Copy these files over to the checkout directory before the make experiment
MAKE_COPY_FILES = {
  'files/Rikerfile-make': 'Rikerfile',
  'files/CMakeLists.txt': 'CMakeLists.txt',
  'files/cmake': 'cmake'
}

# Exclude commands that start with these prefixes from the command count
MAKE_FILTER_CMDS = [
  'rkr-launch',
  'Rikerfile',
  '/bin/sh -c',
  'make -f',
  '/usr/bin/cmake'
]

if __name__ == '__main__':
  if len(sys.argv) < 2:
    print('Usage: {} <path to xz checkout> <mode: rkr, make, or all>'.format(sys.argv[0]))
    exit(1)
  
  project_path = sys.argv[1]
  mode = sys.argv[2]

  if mode not in ['rkr', 'make', 'all']:
    print('Invalid mode. Pass in rkr, make, or all')
    exit(1)

  # Run the riker build experiment
  if mode == 'rkr' or mode == 'all':
    bench.rkr_experiment(project_path, REPO, END_COMMIT, COMMIT_COUNT, RKR_COPY_FILES)

  # Run the make build experiment
  if mode == 'make' or mode == 'all':
    bench.make_experiment(project_path, REPO, END_COMMIT, COMMIT_COUNT, MAKE_COPY_FILES, MAKE_SETUP_CMDS, MAKE_FILTER_CMDS)
