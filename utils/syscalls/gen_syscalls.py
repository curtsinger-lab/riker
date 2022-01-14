#!/usr/bin/env python3

import sys
import os

def show_usage():
  print('Usage: {} <path to syscall-defining include file>'.format(sys.argv[0]))
  print('  amd64: {} /usr/include/x86_64-linux-gnu/asm/unistd_64.h'.format(sys.argv[0]))
  print('  arm64: {} /usr/include/asm-generic/unistd.h'.format(sys.argv[0]))
  exit(1)

if len(sys.argv) != 2:
  show_usage()

if not os.path.isfile(sys.argv[1]):
  printf('File {} does not exist'.format(sys.argv[1]))
  show_usage()

def read_syscall_set(filename):
  result = []
  f = open(filename, 'r')
  for line in f:
    line = line.strip()
    if len(line) == 0:
      continue
    result.append(line)
  return set(result)

# Read in the system calls to trace and skip
TRACE = read_syscall_set(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'TRACE'))
SKIP = read_syscall_set(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'SKIP'))

# Open the include file
f = open(sys.argv[1])

# Prepare the output
out = open('syscalls.hh', 'w')

# Write the output header
print('#pragma once', file=out)
print('', file=out)
print('#if !defined(TRACE)', file=out)
print('#error "syscalls.hh was included without the TRACE macro"', file=out)
print('#endif', file=out)
print('', file=out)

for line in f:
  if not line.startswith('#define __NR'):
    continue

  # Split the line at whitespace
  parts = line.split()

  # Expect three parts: the #define, the syscall name, and the syscall number
  if len(parts) != 3:
    print('Warning: unexpected line "{}"'.format(line))
    continue
  
  # Name the three parts
  (define, name, num) = line.split()

  # If this is the syscall count line, continue
  if name == '__NR_syscalls':
    continue

  # Otherwise, handle the syscall
  if name.startswith('__NR3264_'):
    name = name[9:]
  else:
    name = name[5:]

  try:
    num = int(num)
  except ValueError as e:
    continue

  # Is this a system call we need to trace, skip, or an unknown system call?
  if name in TRACE:
    print('/* {:03} */ TRACE({});'.format(num, name), file=out)
  elif name in SKIP:
    print('/* {:03} */ // skip {}'.format(num, name), file=out)
  else:
    print('Warning: unknown system call {}'.format(name))
    print('  Source line: {}'.format(line))
    print('/* {:03} */ // unrecognized {}'.format(num, name), file=out)
