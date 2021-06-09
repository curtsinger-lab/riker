#!/usr/bin/env python3

import subprocess, sys

def clang_wrapper(args):
    # for arg in args[2:-1]:
    #     # compile each subsequent arg in a separate process
    #     subprocess.call(["clang", "-c", arg])
    #print(args)
    subprocess_arr = []
    c_file_arr = []
    o_file_arr = []
    flags = []
    link = True
    for i in range(2, len(args)):
        arg = args[i]
        if arg == "-o":
            output_name = args[i+1]
        elif ".c" in arg:
            o_file = arg
            #o_file[-1] = "o"
            o_file = o_file[0:-1] + 'o'
            o_file_arr.append(o_file)
            c_file_arr.append(arg)
        elif ".o" in arg:
            o_file_arr.append(arg)
        elif arg == "-c":
            link = False
        elif arg == output_name:
            continue
        else:
            print("Unrecognized flag: " + arg + "\n")
            subprocess.call(args[1:])
            quit
    for arg in c_file_arr:
        o_file = arg
        o_file = o_file[0:-1] + 'o'
        compile_args = ["clang", "-c", "-o", o_file, arg] + flags
        subprocess_arr.append(subprocess.Popen(compile_args))
    exit_codes = [p.wait() for p in subprocess_arr]
    if link:
        link_args = ["clang", "-o", output_name] + o_file_arr
        subprocess.call(link_args)
    
        
    # TODO: account for the case of compiling w/o linking, whether a single .c file or multiple
    # elif arg[2] == "-c": 
    #     compile_args = ["clang", "-c", "-o", ]
        
clang_wrapper(sys.argv)