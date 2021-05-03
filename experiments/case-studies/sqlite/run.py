#!/usr/bin/env python3

import os
import sys

RKR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'rkr'))
REPO = 'https://github.com/sqlite/sqlite'
END_COMMIT = 'c1cace0832fa2af5ab8315e217d708c09d586425' # Tagged release 3.35.5
COMMIT_COUNT = 30

def setup(path, commit):
  print('Setting up working directory at {}'.format(path))

  # Make sure the parent directory exists
  parent_dir = os.path.dirname(path)
  if parent_dir != '' and not os.path.isdir(parent_dir):
    print('Cannot clone to {}: Directory {} does not exist'.format(path, parent_dir))
    exit(1)

  # Clone the repository if it doesn't already exist
  if not os.path.isdir(path):
    rc = os.system('git clone {} {}'.format(REPO, path))
    if rc != 0:
      print('Failed to clone git repository to {}'.format(path))
      exit(1)

  else:
    rc = os.system('cd {}; git reset --hard'.format(path))
    if rc != 0:
      print('Failed to reset git repository at {}'.format(path))
      exit(1)
  
  # Now check out the requested commit
  rc = os.system('cd {}; git checkout {}'.format(path, commit))
  if rc != 0:
    print('Failed to check out commit {}'.format(commit))
    exit(1)
  
  # Copy in the Rikerfile
  rikerfile = os.path.join(os.path.dirname(__file__), 'Rikerfile')
  rc = os.system('cp {} {}'.format(rikerfile, path))
  if rc != 0:
    print('Failed to copy in the Rikerfile')
    exit(1)
  
  # Copy in the Makefile as well
  rikerfile = os.path.join(os.path.dirname(__file__), 'Makefile')
  rc = os.system('cp {} {}'.format(rikerfile, path))
  if rc != 0:
    print('Failed to copy in the Makefile')
    exit(1)
  
  # Clean up any remaining riker build state
  rc = os.system('rm -rf {}'.format(os.path.join(path, '.rkr')))
  if rc != 0:
    printf('Failed to clean up remaining riker build state')
    exit(1)
  
  # Run a full build
  print('Running a full build at commit {}'.format(commit))
  rc = os.system('cd {}; {} --show'.format(path, RKR))
  if rc != 0:
    print('Full build failed')
    exit(1)

def incremental_build(path, commit):
  # Check out the requested commit
  rc = os.system('cd {}; git checkout {}'.format(path, commit))
  if rc != 0:
    print('Failed to check out commit {}'.format(commit))
    exit(1)
  
  # Run the incremental build
  print('Updating build to commit {}'.format(commit))
  rc = os.system('cd {}; {} --show'.format(path, RKR))
  if rc != 0:
    print('Incremental build at commit {} failed'.format(commit))
    exit(1)

if __name__ == '__main__':
  if len(sys.argv) != 2:
    print('Usage: {} <path to sqlite checkout>'.format(sys.argv[0]))
    exit(1)
  
  sqlite_path = sys.argv[1]
  setup(sqlite_path, '{}~{}'.format(END_COMMIT, COMMIT_COUNT))

  for i in reversed(range(0, COMMIT_COUNT)):
    incremental_build(sqlite_path, '{}~{}'.format(END_COMMIT, i))

