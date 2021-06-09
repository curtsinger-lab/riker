#!/usr/bin/env python3

import subprocess, sys, tempfile

def clang_wrapper(args):
    # Initialize arrays
    subprocess_arr = []
    c_file_arr = []
    o_file_arr = []
    flags = []
    link = True
    # Reading the arguments
    for i in range(2, len(args)):
        arg = args[i]
        if arg == "-o":
            output_name = args[i+1]
        elif ".c" in arg:
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
    # Compiling c files 
    tmp = tempfile.TemporaryDirectory()
    for arg in c_file_arr:
        o_file = arg
        o_file = tmp.name + '/' + o_file[0:-1] + 'o'
        o_file_arr.append(o_file)
        compile_args = ["clang", "-c", "-o", o_file, arg] + flags
        subprocess_arr.append(subprocess.Popen(compile_args))
    exit_codes = [p.wait() for p in subprocess_arr]
    # Linking all files
    if link:
        link_args = ["clang", "-o", output_name] + o_file_arr
        subprocess.call(link_args)
    
        
clang_wrapper(sys.argv)