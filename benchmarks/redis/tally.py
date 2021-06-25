#!/usr/bin/env python3

import os

# Import the run configuration settings to get skipped commands
import run

def count_commands(dir, filter=[]):
  counts = {}
  for filename in os.listdir(dir):
    f = open(os.path.join(dir, filename), 'r')
    for line in f:
      line = line.strip()
      if len(line) == 0:
        continue
      
      counted = True
      for pattern in filter:
        if line.startswith(pattern):
          counted = False
      if not counted:
        continue
      
      parts = line.split(maxsplit=1)
      if len(parts) == 0:
        continue
      shortcmd = parts[0]

      shortcmd_parts = shortcmd.rsplit('/', maxsplit=1)
      if len(shortcmd_parts) > 1:
        shortcmd = shortcmd_parts[1]
      
      # Quick hack for consistent naming
      if shortcmd == 'rkr-launch':
        shortcmd = 'rkr'

      if shortcmd not in counts:
        counts[shortcmd] = 1
      else:
        counts[shortcmd] += 1
  return counts


# Count the occurrences of commands in the make and riker builds
make_counts = count_commands('make-commands', run.MAKE_FILTER_CMDS)
rkr_counts = count_commands('rkr-commands')

# Create an output file
out = open('commands.csv', 'w')

print('command,build_tool,count', file=out)

for (cmd, count) in make_counts.items():
  print('{},{},{}'.format(cmd, 'Default', count), file=out)

for (cmd, count) in rkr_counts.items():
  print('{},{},{}'.format(cmd, 'Riker', count), file=out)
