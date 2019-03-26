#!/usr/bin/python3
import re
import sys
from typing import List

class Event:
    def __init__(self, pid: int, fname: str, args: str, retval: int) -> None :
        self.pid = pid
        self.fname = fname
        self.args = args
        self.retval = retval

    def __str__(self) -> str :
        return "pid: {}, function: {}, args: {}, retval: {}".format(self.pid, self.fname, self.args, self.retval)

def usage():
    print("Usage: python3 parse_strace.py <trace.txt>")
    sys.exit(1)

def parse(lines: List[str]) -> List[Event] :
    a: List[Event] = []
    r = '(?P<pid>[0-9]+)\s+(?P<fname>[a-z]+)\((?P<args>.+)\)\s+=\s+(?P<retval>-?[0-9]+)'
    for line in lines:
        m = re.match(r, line, flags=re.I)
        if m:
            pid:    int = int(m.group('pid'))
            fname:  str = m.group('fname')        
            args:   str = m.group('args')
            retval: int = int(m.group('retval'))
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
