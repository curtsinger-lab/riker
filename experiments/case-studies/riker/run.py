#!/usr/bin/env python3

import sys
import bench

REPO = 'git@github.com:curtsinger-lab/riker'
END_COMMIT = 'fbfef5653abe3c44c496ab23fadf233157091ae1'
COMMIT_COUNT = 100

RKR_SETUP_CMDS = [
  'git submodule init',
  'git submodule update',
  'make clean'
]

# Copy these files over to the checkout directory before the rkr experiment
RKR_COPY_FILES = {
  'files/Rikerfile-make-all': 'Rikerfile'
}

# Run these commands in the checkout directory before the make experiment
MAKE_SETUP_CMDS = [
  'git submodule init',
  'git submodule update',
  'make clean'
]

# Copy these files over to the checkout directory before the make experiment
MAKE_COPY_FILES = {
  'files/Rikerfile-make': 'Rikerfile',
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
    print('Usage: {} <path to riker checkout> <mode: rkr, make, or all>'.format(sys.argv[0]))
    exit(1)
  
  project_path = sys.argv[1]
  mode = sys.argv[2]

  if mode not in ['rkr', 'make', 'all']:
    print('Invalid mode. Pass in rkr, make, or all')
    exit(1)

  # Run the riker build experiment
  if mode == 'rkr' or mode == 'all':
    bench.rkr_experiment(project_path, REPO, END_COMMIT, COMMIT_COUNT, RKR_COPY_FILES, RKR_SETUP_CMDS, inject=False)

  # Run the make build experiment
  if mode == 'make' or mode == 'all':
    bench.make_experiment(project_path, REPO, END_COMMIT, COMMIT_COUNT, MAKE_COPY_FILES, MAKE_SETUP_CMDS, MAKE_FILTER_CMDS)
