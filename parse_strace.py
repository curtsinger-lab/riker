#!/usr/bin/python3
import re
import sys
from typing import List, Optional

class Event:
    def __init__(self, pid: int, fname: str, args: List[str], retval: Optional[int]) -> None :
        self.pid: int              = pid
        self.fname: str            = fname
        self.args: List[str]       = args
        self.retval: Optional[int] = retval

    def __str__(self) -> str :
        return "pid: {}, function: {}, args: {}, retval: {}".format(self.pid, self.fname, self.args, self.retval)

# complain
def usage():
    print("Usage: python3 parse_strace.py <trace.txt>")
    sys.exit(1)

# parse argument lists
def argparse(args: str) -> List[str] :
    xs: List[str] = args.split(',')
    xs = [x.lstrip() for x in xs]
    return xs

# parse return values
def rvparse(rv: str) -> Optional[int] :
    if rv == '?':
        return None
    else:
        return int(rv)

# top-level parsing function
def parse(lines: List[str]) -> List[Event] :
    a: List[Event] = []
    # case 1: most lines match this regex
    r = '(?P<pid>[0-9]+)\s+(?P<fname>[a-z0-9_]+)\((?P<args>.*)\)\s+=\s+(?P<retval>-?[0-9?]+)'
    # case 2: other lines start and are finished at later points
    # case 2 start:
    rstart = '(?P<pid>[0-9]+)\s+(?P<fname>[a-z0-9_]+)\((?P<args>.*)unfinished'
    # case 2 end:
    rend = '(?P<pid>[0-9]+)\s+<\.\.\.\s+(?P<fname>[a-z0-9_]+)\s+resumed>\s+(?P<args>.*)\)\s+=\s+(?P<retval>-?[0-9?]+)'
    # case 3: signal
    rsig = '(?P<pid>[0-9]+)\s+---\s+(?P<fname>[A-Z]+)\s+{(?P<args>.+)}\s+---'
    # case 4: exit
    rexit = '(?P<pid>[0-9]+)\s+[+]{3}\s+exited with [0-9]+\s+[+]{3}'
    for i in range(0, len(lines)):
        line = lines[i]
        m = re.match(r, line)

        pid: int = -999
        fname: str = "DEBUG"
        args: List[str] = []
        retval: Optional[int] = -999
        
        # check case 1 
        if m:
            pid    = int(m.group('pid'))
            fname  = m.group('fname')        
            args   = argparse(m.group('args'))
            retval = rvparse(m.group('retval'))
        # check case 2
        else:
            # check case 2 start
            m = re.match(rstart, line)
            if m:
                pid = int(m.group('pid'))
                fname = m.group('fname')
                args = argparse(m.group('args'))
                retval = None
            # check case 2 end
            else:
                m = re.match(rend, line)
                if m:
                    pid    = int(m.group('pid'))
                    fname  = m.group('fname')
                    args   = []
                    retval = rvparse(m.group('retval'))
                # check case 3
                else:
                    m = re.match(rsig, line)
                    if m:
                        pid = int(m.group('pid'))
                        fname = m.group('fname')
                        args = argparse(m.group('args'))
                        retval = None
                    # check case 4
                    else:
                        m = re.match(rexit, line)
                        if m:
                            continue
                        # no match; fail
                        else:                        
                            print("Unable to parse: ", line)
                            sys.exit(1)
        a.append(Event(pid, fname, args, retval))
            
    return a

if __name__ == '__main__':    
    if len(sys.argv) != 2:
        usage()

    with open(sys.argv[1], 'r') as f:
          trace: List[str] = f.readlines()
    trace = [line.strip() for line in trace]

    events: List[Event] = parse(trace)

    for event in events:
        print(event)
