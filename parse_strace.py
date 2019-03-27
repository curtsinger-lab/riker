#!/usr/bin/env python3
import re
import sys
from typing import List, Dict, Optional, Tuple, Any

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

def argparse(s: str) -> List[Any] :
    def p(s: str) -> Tuple[List[Any],int] :
        # always strip leading whitespace
        s2 = s.lstrip()
        diff = len(s) - len(s2)
        s = s2

        #print("DEBUG: HAVE INPUT: {}".format(s))
        
        # base case 1: we're done
        if len(s) == 0:
            #print("DEBUG: end of input.")
            #input()
            return [], diff
        # recursive case 1: looks like a sublist
        elif s[0] == '[':
            #print("DEBUG: SUBLIST")
            #input()
            # parse sublist
            args1, pos1 = p(s[1:])
            #print("DEBUG: GOT BACK SUBLIST: {}, pos = {}".format(args1, pos1 + 1 + diff))
            #input()
            # parse the rest
            args2, pos2 = p(s[pos1 + 1:])
            #print("DEBUG: GOT BACK REST: {}, pos = {}".format(args2, pos1 + pos2 + 1 + diff))
            #input()
            # return + update position
            return [args1] + args2, pos1 + pos2 + 1 + diff
        else:
            #print("DEBUG: NORMAL CASE: INPUT IS: '{}'".format(s))
            # get next token
            parts = s.split(',')
            token, rest = parts[0], ",".join(parts[1:])
            #print("DEBUG: GOT TOKEN: {} AND REST: {}".format(token, rest))

            # account for commas in splits
            diff = diff + len(parts) - 1

            # base case 2: looks like the end of a list
            # stop here
            if token[-1] == ']':
                #print("DEBUG: END OF SUBLIST")
                #input()
                t = token[0:-1]
                args = [] if t == '' else [t]
                # pos = token + 1 if we need to include the comma
                pos = len(token) if len(parts) == 1 else len(token) + 1
                return args, pos

            #print("DEBUG: NORMAL ARG: '{}'".format(token))
            #input()
            # otherwise, the usual recursive case 2: parse the rest
            args, pos = p(rest)
            # return + update position
            return [token] + args, pos + len(token) + diff
    args, pos = p(s)
    return args

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
