#!/usr/bin/env python3

import subprocess, sys, tempfile, os

paths = os.environ['PATH']
path_arr = paths.split(":")
for path in path_arr:
    if "wrappers" in path: 
        path_arr.remove(path)
#print(":".join(path_arr))
os.environ['PATH'] = ":".join(path_arr)

supported_flags = ["-D", "-f", "-I", "-o", "-W", "-pthread", "-g", "-M", "-O", "-std", "--std", "-pedantic", "-m"]
supported_compile_flags = []
supported_linker_flags = ["-L", "-shared", "-Wl", "-l", "-r", "../deps/"]

def clang_wrapper(args):
    # print("Using our Clang wrapper.")
    # Determine compiler and whether we are using C++
    compiler = args[0].split("/")[-1]
    is_cpp = False
    if compiler == "clang" or compiler == "gcc" or compiler == "cc":
        is_cpp = False
    elif compiler == "clang++" or compiler == "g++" or compiler == "c++":
        is_cpp = True
    else:
        print("Error - unrecognized compiler: " + compiler)
        quit() 
    # Initialize arrays
    subprocess_arr = []
    c_file_arr = []
    o_file_arr = []
    compiler_flags = []
    linker_flags = []
    link = True
    temp = True
    output_name = None
    for i in range(1, len(args)):
        arg = args[i]
        if arg == "-nowrapper":
            # print("Not using wrapper.")
            mod_args = [compiler] + args[1:]
            mod_args.remove("-nowrapper")
            subprocess.call(mod_args)
            quit()
        elif arg == "-o":
            output_name = args[i+1]
            if ".o" in output_name:
                temp = False
                link = False
        elif not is_cpp and ".c" == arg[-2:]:
            c_file_arr.append(arg)
        elif is_cpp and ((".cc" == arg[-3:]) or (".cpp" == arg[-4:]) or (".c++" == arg[-4:])):
            c_file_arr.append(arg)
        elif (output_name is not None) and (arg == output_name):
            continue
        elif ".o" == arg[-2:]:
            o_file_arr.append(arg)
        elif arg == "-c":
            link = False
        elif any(arg.startswith(flag) for flag in supported_compile_flags):
            compiler_flags.append(arg) 
        elif any(arg.startswith(flag) for flag in supported_linker_flags):
            linker_flags.append(arg)
        elif any(arg.startswith(flag) for flag in supported_flags):
            # for cflag in supported_compile_flags:
            #     if cflag in arg:
            #         continue
            # for lflag in supported_linker_flags:
            #     if lflag in arg:
            #         continue
            compiler_flags.append(arg)
            linker_flags.append(arg)
        
        else:
            print("Unrecognized flag: " + arg + "\n")
            mod_args = [compiler] + args[1:]
            subprocess.call(mod_args)
            quit()
    # Compiling c files 
    if temp:
        tmp = tempfile.TemporaryDirectory()
    for arg in c_file_arr:
        o_file = arg
        o_file = o_file[0:-1] + 'o'
        if temp:
            o_file = tempfile.mkstemp(suffix=".o", dir=tmp.name)[1]
        o_file_arr.append(o_file)
        compile_args = [compiler, "-c", "-o", o_file, arg] + compiler_flags
        subprocess_arr.append(subprocess.Popen(compile_args))
    exit_codes = any((p.wait() != 0) for p in subprocess_arr)
    # Linking all files
    if exit_codes != 0:
        print(exit_codes)
        quit
    else:
        if link:
            link_args = [compiler, "-o", output_name] + o_file_arr + linker_flags
            subprocess.call(link_args)
    
        
clang_wrapper(sys.argv)