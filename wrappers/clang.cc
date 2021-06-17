#include <iostream>
#include <string>
#include <string.h>
#include <stdio.h>
#include <vector>
#include <algorithm>
#include <array>
#include <stdlib.h>
#include <sstream>
#include <filesystem>
#include <thread>
#include <unistd.h>

using namespace std;

char *convert(const string & s) {
    char *pc = new char[s.size()+1];
    strcpy(pc, s.c_str());
    return pc;
}

int clang_wrapper(vector<string> args) {
    printf("Start of clang wrapper\n");
    vector<string> supported_flags = {"-D", "-f", "-I", "-o", "-W", "-pthread", "-g", "-M", "-O", "-std", "--std", "-pedantic", "-m"};
    vector<string> supported_compile_flags = {};
    vector<string> supported_linker_flags = {"-L", "-shared", "-Wl", "-l", "-r", "../deps/"};

    // Determine compiler and whether we are using C++
    string compiler_str = args.front();
    const char * compiler = compiler_str.c_str();
    const char * last = strrchr(compiler, '/');
    if (last != NULL){
        compiler = last+1;
    }
    printf("Compiler: %s\n", compiler);
    bool is_cpp = false;
    if (!strcmp(compiler, "clang") || !strcmp(compiler, "gcc") || !strcmp(compiler, "cc")) {
        is_cpp = false;
    } else if (!strcmp(compiler, "clang++") || !strcmp(compiler, "g++") || !strcmp(compiler, "c++")) {
        is_cpp = true;
    } else {
        printf("Error - unrecognized compiler: %s\n", compiler);
        return 1;
    }

    // Initialize arrays
    vector<string> subprocess_arr, c_file_arr, o_file_arr, compiler_flags, linker_flags;
    bool link = true, temp = true;
    string output_name;

    // Work through the command
    for (int i = 1; i < args.size(); i++) {
        // printf("i: %d\n", i);
        string argstr = args[i];
        const char * arg = argstr.c_str();
        if (!strcmp(arg, "-nowrapper")){
            // printf("-nowrapper flag detected\n");
            // Call subprocess
            args.erase(args.begin() + i);
            args.erase(args.begin());
            vector<char*> vc;
            transform(args.begin(), args.end(), back_inserter(vc), convert);
            execvp(compiler, &vc[0]);
            return 1;
        } else if (!strcmp(arg, "-o")) {
            // printf("checking -o\n");
            output_name = args[i+1].c_str();
            if (output_name.find(".o") != string::npos) {
                temp = false;
                link = false;
            }
        } else if (!is_cpp && strrchr(arg, '.') && !strcmp(strrchr(arg, '.'), ".c")) {
            // printf("checking .c\n");
            c_file_arr.push_back(argstr);
        } else if (is_cpp && strrchr(arg, '.') &&
                             (!strcmp(strrchr(arg, '.'), ".cc") || 
                              !strcmp(strrchr(arg, '.'), ".cpp") || 
                              !strcmp(strrchr(arg, '.'), ".c++"))) {
            // printf(".cc/cpp/c++ file found");
            c_file_arr.push_back(argstr);
        } else if (!output_name.empty() && (arg == output_name)) {
            // printf("checking output name\n");
            continue;
        } else if (strrchr(arg, '.') && strcmp(strrchr(arg, '.'), ".o") == 0) {
            // printf("checking .o\n");
            o_file_arr.push_back(argstr);
        } else if (!strcmp(arg, "-c")) {
            // printf("checking -c\n");
            link = false;
        } else if (std::any_of(supported_compile_flags.begin(), supported_compile_flags.end(), [&](string flag){return argstr.rfind(flag, 0)==0;})) {
            // printf("checking compiler flags\n");
            compiler_flags.push_back(argstr);  
        } else if (std::any_of(supported_linker_flags.begin(), supported_linker_flags.end(), [&](string flag){return argstr.rfind(flag, 0)==0;})) {
            // printf("checking linker flags\n");
            linker_flags.push_back(argstr);
        } else if (std::any_of(supported_flags.begin(), supported_flags.end(), [&](string flag){return argstr.rfind(flag, 0)==0;})) {
            // printf("checking supported flags\n");
            compiler_flags.push_back(argstr);  
            linker_flags.push_back(argstr);
        } else {
            // Call subprocess
            printf("Unrecognized flag: %s\n", arg);
            args.erase(args.begin() + i);
            args.erase(args.begin());
            vector<char*> vc;
            transform(args.begin(), args.end(), back_inserter(vc), convert);
            execvp(compiler, &vc[0]);
            return 1;
        }
    }
    string tmp;
    if (temp){
        tmp = filesystem::temp_directory_path();
    }
    std::vector<std::thread> threads_arr;
    // Compile each .cc file
    printf("Compiling c files\n");
    for (string arg: c_file_arr){
        printf("arg: %s\n", arg.c_str());
        const char * o_file = arg.c_str();
        string o_file_str;
        if(temp){
            //create temp .o file
            o_file_str = tmp + "/" + std::tmpnam(nullptr) + ".o";
        } else {
            const char * period_index = strrchr(o_file, '.');
            o_file_str = arg;
            o_file_str.replace(period_index-o_file+1, o_file_str.length()-(period_index-o_file), ".o");
        }
        o_file_arr.push_back(o_file_str);
        printf("o_file created: %s\n", o_file_str.c_str());
        vector<string> compile_args = {compiler_str, "-c", "-o", o_file_str};
        string compile_args_str;
        for(string arg: compile_args){
            compile_args_str += arg + " ";
        }
        for(string arg: compiler_flags){
            compile_args_str += arg + " ";
        }
        printf("Compiling in new thread\n");
        std::thread new_thread([](string to_run){system(&to_run[0]);}, compile_args_str);
        threads_arr.push_back(move(new_thread));
        
    }
    printf("Waiting for threads to join\n");
    for (std::thread & subprocess_thread : threads_arr) {
        subprocess_thread.join();
    }
    // Link .o files.
    printf("Linking all files\n");
    if (link){
        string linkstr = compiler_str + " -o " + output_name;
        for (string o_file : o_file_arr) {
            linkstr += (" " + o_file);
        } for (string linker_flag : linker_flags) {
            linkstr += (" " + linker_flag);
        }
        system(&linkstr[0]);
    }
    return 1;
}

int main(int argc, char* argv[]){
    printf("Using our CLANG\n");
    // Remove wrappers from the PATH
    char * pathC = getenv("PATH");
    string paths = string(pathC);
    // split path into vector using delimiter ':'
    vector<string> path_arr;
    string tmp;
    stringstream ss(paths);
    while (getline(ss, tmp, ':')) 
        path_arr.push_back(tmp);
    for (vector<string>::iterator it = path_arr.begin(); it != path_arr.end();) {
        if ((*it).find("wrappers") != string::npos) {
            it = path_arr.erase(it);
        } else 
            ++it;
    }
    paths.clear();
    for (const auto &path : path_arr) paths += path;
    printf("new path: %s\n", paths.c_str());
    setenv("PATH", paths.c_str(), 1);

    //int n = sizeof(argv) / sizeof(argv[0]);
    vector<string> args(argv, argv+argc);
    return clang_wrapper(args);
}
            
        