#!/usr/bin/env python3

import os
import subprocess
import sys
import time

TESTDIR = "tests"

# List the tests directory
testdirs = os.listdir(TESTDIR)
testdirs.sort()

# Get the longest test directory name
longest = max(map(len, testdirs))

# Set up an environment map to run the tests
testenv = os.environ.copy()
testenv['DODO'] = os.path.join('..', '..', 'dodo')

# If any test run exits with a non-zero code, remember it here
exitcode = 0

# Keep track of passed, skipped, and failed tests
passed = 0
skipped = 0
failed = 0

# Keep track of runtime
runtime = 0.0

# Run tests in each directory
for d in testdirs:
  # If the entry in the test directory is not a dir, skip it
  if not os.path.isdir(os.path.join(TESTDIR, d)):
    continue

  # Get a list of tests
  tests = []
  for t in os.listdir(os.path.join(TESTDIR, d)):
    if t.endswith('.t'):
      tests.append(os.path.join(TESTDIR, d, t))

  # If there are no tests, skip this iteration
  if len(tests) == 0:
    continue

  # Run the tests
  start_time = time.time()
  p = subprocess.Popen(['cram', '--quiet'] + tests, env=testenv, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

  # Print the test directory name
  sys.stdout.write(('{:>'+str(longest)+'} ').format(d))
  sys.stdout.flush()

  # Print output until the first newline
  c = p.stdout.read(1).decode()
  while c != '\n' and len(c) == 1:
    if c == '.':
      passed += 1
    elif c == 's':
      skipped += 1
    else:
      failed += 1

    sys.stdout.write(c)
    sys.stdout.flush()
    c = p.stdout.read(1).decode()
  sys.stdout.write('\n')
  
  # Consume remaining output
  p.communicate()

  # Update the runtime
  runtime += time.time() - start_time

  # Check the return code
  if p.returncode != 0:
    exitcode = p.returncode

tests = passed + skipped + failed
print("Ran {} tests, {} passed, {} skipped, {} failed.".format(tests, passed, skipped, failed))
print("Tests completed in {:.2f}s".format(runtime))

exit(exitcode)
