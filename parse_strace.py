#!/usr/bin/env python3
import re
import sys
from typing import List, Dict, Optional

class Event:
    def __init__(self, pid: int, name: str, args: List[str], retval: Optional[int]) -> None :
        self.pid: int              = pid
        self.name: str             = name
        self.args: List[str]       = args
        self.retval: Optional[int] = retval

    def __str__(self) -> str :
        return "pid: {}, event: {}, args: {}, retval: {}".format(self.pid, self.name, self.args, self.retval)

# complain
def usage():
    print("Usage: python3 parse_strace.py <trace.txt>")
    sys.exit(1)

# parse argument lists
def argparse(args: str) -> List[str] :
    xs: List[str] = args.split(',')
    # remove leading whitespace
    xs = [x.lstrip() for x in xs]
    # remove empty strings
    xs = list(filter(None, xs))
    return xs

# parse return values
def rvparse(rv: str) -> Optional[int] :
    if rv == '?':
        return None
    else:
        return int(rv)

def parse_syscall(line: str) -> Optional[Event] :
    r = '(?P<pid>[0-9]+)\s+(?P<name>[a-z0-9_]+)\((?P<args>.*)\)\s+=\s+(?P<retval>-?[0-9?]+)'
    m = re.match(r, line)
        
    if m:
        pid    = int(m.group('pid'))
        name   = m.group('name')        
        args   = argparse(m.group('args'))
        retval = rvparse(m.group('retval'))
        return Event(pid, name, args, retval)
    else:
        return None

def parse_syscall_prefix(line: str) -> Optional[Event] :
    r = '(?P<pid>[0-9]+)\s+(?P<name>[a-z0-9_]+)\((?P<args>.*)<unfinished'
    m = re.match(r, line)
    if m:
        pid = int(m.group('pid'))
        name = m.group('name')
        args = argparse(m.group('args'))
        return Event(pid, name, args, None)
    else:
        return None

def parse_syscall_suffix(line: str) -> Optional[Event] :
    r = '(?P<pid>[0-9]+)\s+<\.\.\.\s+(?P<name>[a-z0-9_]+)\s+resumed>\s+(?P<args>.*)\)\s+=\s+(?P<retval>-?[0-9?]+)'
    m = re.match(r, line)
    if m:
        pid    = int(m.group('pid'))
        name  = m.group('name')
        retval = rvparse(m.group('retval'))
        return Event(pid, name, [], retval)
    else:
        return None

def parse_signal(line: str) -> Optional[Event] :
    r = '(?P<pid>[0-9]+)\s+---\s+(?P<name>[A-Z]+)\s+{(?P<args>.+)}\s+---'
    m = re.match(r, line)
    if m:
        pid = int(m.group('pid'))
        name = m.group('name')
        args = argparse(m.group('args'))
        return Event(pid, name, args, None)
    else:
        return None

def parse_exit(line: str) -> bool :
    r = '(?P<pid>[0-9]+)\s+[+]{3}\s+exited with [0-9]+\s+[+]{3}'
    m = re.match(r, line)
    if m:
        return True
    else:
        return False
                    
# top-level parsing function
def parse(lines: List[str]) -> List[Event] :
    a: List[Event] = []
    starts: Dict[int,Event] = {}

    for i in range(0, len(lines)):
        line = lines[i]

        # check case 1: most lines are ordinary syscalls
        evt = parse_syscall(line)
        if evt:
            a.append(evt)
            
        # check case 2
        else:
            # check case 2 start
            evt = parse_syscall_prefix(line)
            if evt:
                starts[evt.pid] = evt
            # check case 2 end
            else:
                evt = parse_syscall_suffix(line)
                if evt:
                    prev = starts[evt.pid]
                    del starts[evt.pid]
                    a.append(Event(evt.pid, evt.name, prev.args, evt.retval))
                # check case 3: signals
                else:
                    evt = parse_signal(line)
                    if evt:
                        a.append(evt)
                    # check case 4: exit
                    elif parse_exit(line):
                        continue
                    # no match; fail
                    else:                        
                        print("Unable to parse: ", line)
                        sys.exit(1)
            
    return a

def parse_file(filename: str) -> List[Event]:
    with open(sys.argv[1], 'r') as f:
          trace: List[str] = f.readlines()
    trace = [line.strip() for line in trace]

    return parse(trace)

if __name__ == '__main__':    
    if len(sys.argv) != 2:
        usage()
    
    for event in parse_file(sys.argv[1]):
        print(event)
