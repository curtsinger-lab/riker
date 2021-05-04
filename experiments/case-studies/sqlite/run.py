#!/usr/bin/env python3

import sys
import bench

REPO = 'https://github.com/sqlite/sqlite'
END_COMMIT = 'c1cace0832fa2af5ab8315e217d708c09d586425' # Tagged release 3.35.5
COMMIT_COUNT = 100

# Run these commands in the checkout directory before the rkr experiment
RKR_SETUP_CMDS = []

# Copy these files over to the checkout directory before the rkr experiment
RKR_COPY_FILES = {
  'files/Rikerfile': 'Rikerfile',
  'files/Makefile': 'Makefile'
}

# Run these commands in the checkout directory before the make experiment
MAKE_SETUP_CMDS = [
  'make clean'
]

# Copy these files over to the checkout directory before the make experiment
MAKE_COPY_FILES = {
  'files/Rikerfile-make': 'Rikerfile',
  'files/Makefile': 'Makefile'
}

# Exclude commands that start with these prefixes from the command count
MAKE_FILTER_CMDS = [
  'rkr-launch',
  'Rikerfile',
  '/bin/sh -c',
  'make -f'
]

if __name__ == '__main__':
  if len(sys.argv) < 2:
    print('Usage: {} <path to sqlite checkout> <mode: rkr, make, or all>'.format(sys.argv[0]))
    exit(1)
  
  project_path = sys.argv[1]
  mode = sys.argv[2]

  if mode not in ['rkr', 'make', 'all']:
    print('Invalid mode. Pass in rkr, make, or all')
    exit(1)

  # Run the riker build experiment
  if mode == 'rkr' or mode == 'all':
    bench.rkr_experiment(project_path, REPO, END_COMMIT, COMMIT_COUNT, RKR_COPY_FILES, RKR_SETUP_CMDS)

  # Run the make build experiment
  if mode == 'make' or mode == 'all':
    bench.make_experiment(project_path, REPO, END_COMMIT, COMMIT_COUNT, MAKE_COPY_FILES, MAKE_SETUP_CMDS, MAKE_FILTER_CMDS)
