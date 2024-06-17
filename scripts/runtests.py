#!/usr/bin/env python3

import os
import subprocess
import sys
import time

RKR_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
TESTDIR = os.path.join(RKR_DIR, "tests")

# List the tests directory
testdirs = os.listdir(TESTDIR)
testdirs.sort()

# Get the longest test directory name
longest = max(map(len, testdirs))

# Set up an environment map to run the tests
testenv = os.environ.copy()

# Remove make flags from the test environment (this may be set if tests are run from make)
if 'MAKEFLAGS' in testenv:
  del testenv['MAKEFLAGS']

# Make sure PATH is set
if 'PATH' not in testenv:
  print('PATH environment variable is not set. Stopping tests now.')
  sys.exit(1)

# Make sure we can find an executable rkr somewhere in PATH
found_rkr_in = None
for p in testenv['PATH'].split(':'):
  if os.access(os.path.join(p, 'rkr'), os.X_OK):
    found_rkr_in = p
    break

# Bail if there's no rkr
if not found_rkr_in:
  print('An executable rkr command was not found. Make sure it is available in the PATH (e.g. set PATH=$PWD/debug/bin:$PATH).')
  print('You might just want to run `make test-debug` or `make test-release` instead.')
  sys.exit(1)
else:
  print('Running tests using rkr in {}'.format(found_rkr_in))

# If any test run exits with a non-zero code, remember it here
exitcode = 0

# Keep track of passed, skipped, and failed tests
passed = 0
skipped = 0
failed = 0

# Keep track of runtime
runtime = 0.0

# Keep a list of the paths to all tests
tests = []

# Keep a list of pairs of (test dir name, test count)
test_counts = []

# Build a list of all the tests
for d in testdirs:
  # If the entry in the test directory is not a dir, skip it
  if not os.path.isdir(os.path.join(TESTDIR, d)):
    continue

  # Get a list of tests
  local_tests = []
  for t in os.listdir(os.path.join(TESTDIR, d)):
    if t.endswith('.t'):
      local_tests.append(os.path.join(TESTDIR, d, t))

  # If there are no tests, skip this iteration
  if len(local_tests) == 0:
    continue

  # Sort the list of tests
  local_tests.sort()

  # Add the current list of tests to the global list
  tests += local_tests

  # Record the test directory name and number of tests
  test_counts += [(d, len(local_tests))]

# Now launch all the tests at once
start_time = time.time()
p = subprocess.Popen(['cram'] + tests, env=testenv, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

# Loop over each of the test directories
for d, count in test_counts:
  # Print the test directory name
  sys.stdout.write(('{:>'+str(longest)+'} ').format(d))
  sys.stdout.flush()

  # Keep track of any test failure messages
  messages = []

  # Print test outcomes until we've hit `count` tests for this directory
  for i in range(0, count):
    c = p.stdout.read(1).decode()
    if c == '.':
      passed += 1
    elif c == 's':
      skipped += 1
    elif c == '!':
      failed += 1
      message = ''
      while p.stdout.peek(1).decode() not in ['.', '!', 's']:
        message += p.stdout.read(1).decode()
      messages.append(message)
    else:
      c = '?'
      failed += 1

    sys.stdout.write(c)
    sys.stdout.flush()
  sys.stdout.write('\n')

  for message in messages:
    sys.stdout.write(message + '\n')
  
# Consume remaining output
p.communicate()

runtime = time.time() - start_time

# Check the return code
if p.returncode != 0:
  exitcode = p.returncode

tests = passed + skipped + failed
print("Ran {} tests, {} passed, {} skipped, {} failed.".format(tests, passed, skipped, failed))
print("Tests completed in {:.2f}s".format(runtime))

exit(exitcode)
