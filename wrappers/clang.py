#!/usr/bin/env python3

import subprocess, sys

clang_wrapper(sys.argv)

def clang_wrapper(args):
    # for arg in args[2:-1]:
    #     # compile each subsequent arg in a separate process
    #     subprocess.call(["clang", "-c", arg])
    if args[1] == "-o"
        output_name = args[2]
        for arg in args[2:-1]:
            if ".c" in arg:
                subprocess.run(["clang", "-c", arg[-2:-1]=".o", arg])
        