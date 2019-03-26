#!/usr/bin/python3
import re
import sys

def usage():
    print("Usage: python3 parse_strace.py <trace.txt>")
    sys.exit(1)

def parse(input: str):
    r = '(?P<pid>[0-9]+)\s+(?P<function>[a-z]+)'
    m = re.match(r, input, flags=re.I)
    if m :
        print("PID: {}, function: {}".format(m.group('pid'), m.group('function')))
    else:
        print("didn't get stuff")
    return None

if __name__ == '__main__':    
    if len(sys.argv) != 2:
        usage()

    with open(sys.argv[1], 'r') as f:
          trace = f.read().replace('\n', '')

    parse(trace)
