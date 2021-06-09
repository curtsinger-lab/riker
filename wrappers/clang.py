#!/usr/bin/env python3

import subprocess, sys

def clang_wrapper(args):
    # for arg in args[2:-1]:
    #     # compile each subsequent arg in a separate process
    #     subprocess.call(["clang", "-c", arg])
    #print(args)
    if args[2] == "-o":
        output_name = args[3]
        subprocess_arr = []
        o_file_arr = []
        #print(args[4:])
        for arg in args[4:]:
            if ".c" in arg:
                o_file = arg
                #o_file[-1] = "o"
                o_file = o_file[0:-1] + 'o'
                o_file_arr.append(o_file)
                subprocess_arr.append(subprocess.Popen(["clang", "-c", "-o", o_file, arg]))
            elif ".o" in arg:
                o_file_arr.append(arg)
        exit_codes = [p.wait() for p in subprocess_arr]
        link_args = ["clang", "-o", output_name] + o_file_arr
        # link_args.append(o_file_arr)
        #print(link_args)
        subprocess.call(link_args)
    # TODO: account for the case of compiling w/o linking, whether a single .c file or multiple
    # elif arg[2] == "-c": 
    #     compile_args = ["clang", "-c", "-o", ]
        
clang_wrapper(sys.argv)