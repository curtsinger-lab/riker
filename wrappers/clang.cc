#include <iostream>
#include <string>
#include <string.h>
#include <stdio.h>
#include <vector>
#include <algorithm>
#include <array>
#include <stdlib.h>
#include <sstream>

using namespace std;

int clang_wrapper(vector<string> args) {
    vector<string> supported_flags = {"-D", "-f", "-I", "-o", "-W", "-pthread", "-g", "-M", "-O", "-std", "--std", "-pedantic", "-m"};
    vector<string> supported_compile_flags = {};
    vector<string> supported_linker_flags = {"-L", "-shared", "-Wl", "-l", "-r", "../deps/"};

    // Determine compiler and whether we are using C++
    string compiler2 = args.front();
    const char * compiler = compiler2.c_str();
    const char * last = strrchr(compiler, '/');
    if (last != NULL){
        compiler = last+1;
    }
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

    for (int i = 1; i < args.size(); i++) {
        string argstr = args[i];
        const char * arg = argstr.c_str();
        if (!strcmp(arg, "-nowrapper")){
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
            setenv("PATH", paths.c_str(), 1);
            // TODO: Call subprocess
        } else if (!strcmp(arg, "-o")) {
            output_name = args[i+1].c_str();
            if (output_name.find(".o") != string::npos) {
                temp = false;
                link = false;
            }
        } else if (!is_cpp && !strcmp(strrchr(arg, '.'), ".c")) {
            c_file_arr.push_back(argstr);
        } else if (is_cpp && (!strcmp(strrchr(arg, '.'), ".cc") || 
                              !strcmp(strrchr(arg, '.'), ".cpp") || 
                              !strcmp(strrchr(arg, '.'), ".c++"))) {
            c_file_arr.push_back(argstr);
        } else if (!output_name.empty() && (arg == output_name)) {
            continue;
        } else if (strcmp(strrchr(arg, '.'), ".o") == 0) {
            o_file_arr.push_back(argstr);
        } else if (arg == "-c") {
            link = false;
        } else if (std::any_of(supported_compile_flags.begin(), supported_compile_flags.end(), [&](string flag){return argstr.rfind(flag, 0)==0;})) {
            compiler_flags.push_back(argstr);  
        } else if (std::any_of(supported_linker_flags.begin(), supported_linker_flags.end(), [&](string flag){return argstr.rfind(flag, 0)==0;})) {
            linker_flags.push_back(argstr);
        } else if (std::any_of(supported_flags.begin(), supported_flags.end(), [&](string flag){return argstr.rfind(flag, 0)==0;})) {
            compiler_flags.push_back(argstr);  
            linker_flags.push_back(argstr);
        } else {
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
            setenv("PATH", paths.c_str(), 1);
            
        }
    }
    if(temp){
        //create temporary directory
    }
    // Compile each .cc file
    for (string arg: c_file_arr){
        string o_file = arg;
        if(temp){
            //create temp .o file
        }
    }
    // Link .o files.
    if (link){

    }
    return 1;
}

int main(int argc, char* argv[]){
    int n = sizeof(argv) / sizeof(argv[0]);
    vector<string> args(argv, argv+n);
    return clang_wrapper(args);
}
            
        