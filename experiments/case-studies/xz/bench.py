import os
import shutil
import time

RKR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'rkr'))

def checkout(path, commit):
  # Check out the requested commit
  rc = os.system('cd {}; git checkout -f -q {}'.format(path, commit))
  if rc != 0:
    print('Failed to check out commit {}'.format(commit))
    exit(1)

def setup(path, repo, commit, copy_files={}, setup_cmds=[]):
  # Make sure the parent directory exists
  parent_dir = os.path.dirname(path)
  if parent_dir != '' and not os.path.isdir(parent_dir):
    print('Cannot clone to {}: Directory {} does not exist'.format(path, parent_dir))
    exit(1)

  # Clone the repository if it doesn't already exist
  if not os.path.isdir(path):
    rc = os.system('git clone -q {} {}'.format(repo, path))
    if rc != 0:
      print('Failed to clone git repository to {}'.format(path))
      exit(1)

  else:
    rc = os.system('cd {}; git reset --hard -q'.format(path))
    if rc != 0:
      print('Failed to reset git repository at {}'.format(path))
      exit(1)
  
  # Now check out the requested commit
  checkout(path, commit)
  
  # Copy in the requested files
  for src_name, dest_name in copy_files.items():
    src = os.path.join(os.path.dirname(__file__), src_name)
    dest = os.path.join(path, dest_name)
    rc = os.system('cp -r {} {}'.format(src, dest))
    if rc != 0:
      print('Failed to copy {} to {}'.format(src, dest))
      exit(1)
  
  # Clean up any remaining riker build state
  rc = os.system('rm -rf {}'.format(os.path.join(path, '.rkr')))
  if rc != 0:
    printf('Failed to clean up remaining riker build state')
    exit(1)
  
  # Run the required setup commands
  for cmd in setup_cmds:
    rc = os.system('cd {}; {}'.format(path, cmd))
    if rc != 0:
      print('Failed to run setup command {}'.format(cmd))
      exit(1)

def rkr_build(path, commands_file):
  commands_file = os.path.abspath(commands_file)

  start_time = time.perf_counter()
  rc = os.system('cd {}; {} --show-full -o {} 2> /dev/null'.format(path, RKR, commands_file))
  end_time = time.perf_counter()
  if rc != 0:
    print('Build failed')
    exit(1)
  
  # Get the size of the riker database
  try:
    db_size = os.path.getsize('{}/.rkr/db'.format(path))
  except:
    print('Failed to get the size of the riker database')
    exit(1)
  
  # Compute the size of the riker cache
  try:
    cache_size = 0
    cache_count = 0;
    for (dirname, subdirs, files) in os.walk('{}/.rkr/cache'.format(path)):
      for f in files:
        cache_count += 1
        cache_size += os.path.getsize(os.path.join(dirname, f))
  except:
    printf('Failed to get the size of the riker cache')
    exit(1)
  
  return (end_time - start_time, db_size, cache_size, cache_count)

def rkr_audit(path, commands_file):
  commands_file = os.path.abspath(commands_file)

  start_time = time.perf_counter()
  rc = os.system('cd {}; {} audit -o {} 2> /dev/null'.format(path, RKR, commands_file))
  end_time = time.perf_counter()
  if rc != 0:
    print('Build failed')
    exit(1)
  
  return end_time - start_time

def make(path):
  start_time = time.perf_counter()
  rc = os.system('cd {}; make --quiet 2>&1 > /dev/null'.format(path))
  end_time = time.perf_counter()
  if rc != 0:
    print('Make build failed')
    exit(1)
  return end_time - start_time

def count_lines(path):
  f = open(path, 'r')
  count = 0
  for line in f:
    if len(line.strip()) > 0:
      count += 1
  return count

# Count lines in a file (a list of commands) but exclude lines with known prefixes
def count_lines_filtered(path, filter):
  f = open(path, 'r')
  count = 0
  for line in f:
    counted = True

    # Make sure the line isn't empty
    if len(line.strip()) == 0:
      counted = False
    
    # Check to see if the line matches any of the filtered prefixes
    for pattern in filter:
      if line.startswith(pattern):
        counted = False
    
    if counted:
      count += 1
  return count

def rkr_experiment(project_path, repo, end_commit, commit_count, copy_files, setup_cmds=[]):
  # Set up the repository for our first build
  commit = '{}~{}'.format(end_commit, commit_count)
  setup(project_path, repo, commit, copy_files, setup_cmds)

  # Save the old rkr-commands directory and create a new one
  if os.path.isdir('rkr-commands'):
    if os.path.isdir('.old-rkr-commands'):
      shutil.rmtree('.old-rkr-commands')
    os.rename('rkr-commands', '.old-rkr-commands')
  os.mkdir('rkr-commands')
  
  # Save the old data file if it exists
  if os.path.isfile('rkr.csv'):
    os.rename('rkr.csv', '.old-rkr.csv')

  # Open a csv file to write data to
  csv = open('rkr.csv', 'w')
  print('build,commands,runtime,db_size,cache_size,cache_count', file=csv)

  for i in range(0, commit_count + 1):
    # Check out the next revision
    commit_distance = commit_count - i
    commit = '{}~{}'.format(end_commit, commit_distance)
    print('Checking out commit {}'.format(commit))
    checkout(project_path, commit)

    # Run the incremental build
    print('Running incremental build at commit {}'.format(commit))
    cmds_path = os.path.join('rkr-commands', '{:0>3}'.format(i))
    (runtime, db_size, cache_size, cache_count) = rkr_build(project_path, cmds_path)
    commands = count_lines(cmds_path)
    print('{},{},{},{},{},{}'.format(i, commands, runtime, db_size, cache_size, cache_count), file=csv)

def make_experiment(project_path, repo, end_commit, commit_count, copy_files, setup_cmds=[], cmd_filter=[]):
  print('Counting commands in a make build')

  # Set up the repository for our first build
  commit = '{}~{}'.format(end_commit, commit_count)
  setup(project_path, repo, commit, copy_files, setup_cmds)

  # Save the old rkr-commands directory and create a new one
  if os.path.isdir('make-commands'):
    if os.path.isdir('.old-make-commands'):
      shutil.rmtree('.old-make-commands')
    os.rename('make-commands', '.old-make-commands')
  os.mkdir('make-commands')

  audit_times = []
  command_counts = []
  filtered_command_counts = []

  for i in range(0, commit_count + 1):
    # Check out the next revision
    commit_distance = commit_count - i
    commit = '{}~{}'.format(end_commit, commit_distance)
    print('Checking out commit {}'.format(commit))
    checkout(project_path, commit)

    # Run the incremental build
    print('Running build at commit {}'.format(commit))
    cmds_path = os.path.join('make-commands', '{:0>3}'.format(i))
    audit_time = rkr_audit(project_path, cmds_path)
    commands = count_lines(cmds_path)
    filtered_commands = count_lines_filtered(cmds_path, cmd_filter)

    audit_times.append(audit_time)
    command_counts.append(commands)
    filtered_command_counts.append(filtered_commands)
  
  # Reverse the lists of results from the first phase so we can pop each item
  audit_times.reverse()
  command_counts.reverse()
  filtered_command_counts.reverse()

  print('Running uninstrumented make builds')

  # Set up the repository for our first build
  commit = '{}~{}'.format(end_commit, commit_count)
  setup(project_path, repo, commit, copy_files, setup_cmds)

  # Save the old data file if it exists
  if os.path.isfile('make.csv'):
    os.rename('make.csv', '.old-make.csv')

  # Open a csv file to write data to
  csv = open('make.csv', 'w')
  print('build,commands,filtered_commands,runtime,audit_runtime', file=csv)

  for i in range(0, commit_count + 1):
    # Check out the next revision
    commit_distance = commit_count - i
    commit = '{}~{}'.format(end_commit, commit_distance)
    print('Checking out commit {}'.format(commit))
    checkout(project_path, commit)

    # Run the incremental build
    print('Running build at commit {}'.format(commit))
    runtime = make(project_path)
    commands = command_counts.pop()
    filtered_commands = filtered_command_counts.pop()
    audit_time = audit_times.pop()
    print('{},{},{},{},{}'.format(i, commands, filtered_commands, runtime, audit_time), file=csv)

