#!/usr/bin/env python3

import sys
import bench

REPO = 'https://github.com/redis/redis'
END_COMMIT = 'd96f47cf06b1cc24b82109e0e87ac5428517525a'
COMMIT_COUNT = 100

RKR_COPY_FILES = {'files/Rikerfile': 'Rikerfile'}

MAKE_SETUP_CMDS = [
  'make clean'
]

MAKE_COPY_FILES = {
  'files/Rikerfile-make': 'Rikerfile'
}

MAKE_FILTER_CMDS = [
  'rkr-launch',
  'Rikerfile',
  '/bin/sh -c',
  'make -f'
]

if __name__ == '__main__':
  if len(sys.argv) < 2:
    print('Usage: {} <path to redis checkout>'.format(sys.argv[0]))
    exit(1)
  
  project_path = sys.argv[1]

  # Run the riker build experiment
  bench.rkr_experiment(project_path, REPO, END_COMMIT, COMMIT_COUNT, RKR_COPY_FILES)

  # Run the make build experiment
  bench.make_experiment(project_path, REPO, END_COMMIT, COMMIT_COUNT, MAKE_COPY_FILES, MAKE_SETUP_CMDS, MAKE_FILTER_CMDS)
