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

# handy debugger function
def dbg(message: str):
    ENABLE_DBG = False
    if ENABLE_DBG:
        print(message)
        input()
    
# complain
def usage():
    print("Usage: python3 parse_strace.py <trace.txt>")
    sys.exit(1)
    
def argparse(s: str) -> List[Any] :
    # recursive list parser
    def parse_list_start(s: str) -> Tuple[List[Any],int] :
        dbg("DEBUG: SUBLIST")
        # parse sublist
        args1, pos1 = p(s[1:])
        dbg("DEBUG: GOT BACK SUBLIST: {}, pos = {}".format(args1, pos1 + 1))        
        # parse the rest
        dbg("DEBUG: parse rest starting with: {}".format(s[pos1 + 1:] + "..."))
        args2, pos2 = p(s[pos1 + 1:])
        dbg("DEBUG: GOT BACK REST: {}, pos = {}".format(args2, pos1 + pos2 + 1))
        # return + update position
        return [args1] + args2, pos1 + pos2 + 1

    def parse_list_end(s: str) -> Tuple[List[Any], int] :
        dbg("DEBUG: END OF SUBLIST")
                
        t = s[0:-1]
        args = [] if t == '' else [t]
        return args, len(s)
    
    # quoted string parser
    def parse_quoted(s: str) -> Tuple[List[Any],int] :
        dbg("DEBUG: QUOTED STRING: '{}'".format(s))
            
        # find the entire string
        r = '^"(?P<arg>.*?)"($|[,\]])'
        m = re.match(r, s)
        if m:
            token = m.group('arg')

            # consume comma if quoted string ends in comma
            toks = 3 if (len(s) > len(token) + 2 and s[len(token) + 2] == ',') else 2
            rest = s[toks + len(token):]
            pos1 = toks + len(token)
            dbg("DEBUG: GOT QUOTED TOKEN '{}'".format(token))
                
            args, pos2 = p(rest)
            return [token if len(token) > 0 else None] + args, pos1 + pos2
        else:
            dbg("Malformed string.")
            exit(1)

    # returns the next token and remainder of input
    def tokenize(s: str) -> Tuple[str,str] :
        parts = s.split(',', 1)
        token, rest = parts[0], parts[1] if len(parts) > 1 else ''
        dbg("DEBUG: GOT TOKEN: {} AND REST: {}".format(token, rest))
        return token, rest
            
    # main recursive parser
    def p(s: str) -> Tuple[List[Any],int] :
        # always strip leading whitespace
        s2 = s.lstrip()
        diff = len(s) - len(s2)
        s = s2

        dbg("DEBUG: HAVE INPUT: {}".format(s))
        
        # base case 1: we're done
        if len(s) == 0:
            dbg("DEBUG: end of input.")
            return [], diff
        # recursive case 1: looks like a sublist
        elif s[0] == '[':
            args, pos = parse_list_start(s)
            return args, pos + diff
        # recursive case 2: a quoted string
        elif s[0] == '"':
            args, pos = parse_quoted(s)
            return args, pos + diff
        else:
            dbg("DEBUG: NORMAL CASE: INPUT IS: '{}'".format(s))
            
            # get next token
            token, rest = tokenize(s)

            # account for comma in split
            diff = diff + 1

            # base case 2: looks like the end of a list
            # stop here
            if token[-1] == ']':
                args,pos = parse_list_end(s)
                # consume comma if list ends in comma
                pos = (pos if len(rest) > 0 else pos + 1) + diff
                return args, pos

            dbg("DEBUG: NORMAL ARG: '{}'".format(token))
            
            # otherwise, the usual recursive case 3: parse the rest
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
#        try:
        pid    = int(m.group('pid'))
        name   = m.group('name')        
        args   = argparse(m.group('args'))
        retval = rvparse(m.group('retval'))
#        except:
#            print("****************** DIED ***********************")
#            print(line)
#            exit(1)
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
