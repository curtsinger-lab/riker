#include <algorithm>
#include <array>
#include <filesystem>
#include <iostream>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

using namespace std;

// Helper function for converting vector of strings to array of char*
char* convert(const string& s) {
  char* pc = new char[s.size() + 1];
  strcpy(pc, s.c_str());
  return pc;
}

// Main function that divides up the work
int clang_wrapper(vector<string> args) {
  // std::cout << "Start of clang wrapper" << endl;
  vector<string> supported_flags = {"-print", "-D", "-f", "-o", "-W","-pthread", "-g", "-M", "-O", "-std", "--std", "-pedantic", "-m"};
  vector<string> supported_compile_flags = {};
  vector<string> supported_linker_flags = {"-L", "-shared", "-Wl", "-l", "-r"};

  // Determine compiler and whether we are using C++
  string compiler_str = args.front();
  const char* compiler = compiler_str.c_str();
  const char* last = strrchr(compiler, '/');
  if (last != NULL) {
    compiler = last + 1;
  }
  // std::cout << "Compiler: " << compiler << endl;
  bool is_cpp = false;
  if (!strcmp(compiler, "clang") || !strcmp(compiler, "gcc") || !strcmp(compiler, "cc")) {
    is_cpp = false;
  } else if (!strcmp(compiler, "clang++") || !strcmp(compiler, "g++") || !strcmp(compiler, "c++")) {
    is_cpp = true;
  } else {
    std::cout << "Error - unrecognized compiler:" << compiler << endl;
    return 1;
  }

  // Initialize arrays
  vector<string> subprocess_arr, c_file_arr, o_file_arr, a_file_arr, compiler_flags, linker_flags;
  bool link = true, temp = true, print = false;
  string output_name, include_path;

  // Work through the command
  for (int i = 1; i < args.size(); i++) {
    string argstr = args[i];
    const char* arg = argstr.c_str();
    if (!strcmp(arg, "-nowrapper")) {
      // Call subprocess
      args.erase(args.begin() + i);
      // args.erase(args.begin());
      vector<char*> vc;
      transform(args.begin(), args.end(), back_inserter(vc), convert);
      cout << compiler << " " << &vc[0] << endl;
      if (execvp(compiler, &vc[0]) == -1) std::cout << "nowrapper failed" << endl;
      return 1;
    } else if (!strcmp(arg, "-print")) {
      print = true;
    } else if (!strcmp(arg, "-o")) {
      // printf("checking -o\n");
      output_name = args[i + 1].c_str();
      if (output_name.find(".o") != string::npos) {
        temp = false;
        link = false;
      }
    } else if (!is_cpp && strrchr(arg, '.') && !strcmp(strrchr(arg, '.'), ".c")) {
      // printf("checking .c\n");
      c_file_arr.push_back(argstr);
    } else if (is_cpp && strrchr(arg, '.') &&
               (!strcmp(strrchr(arg, '.'), ".cc") || !strcmp(strrchr(arg, '.'), ".cpp") ||
                !strcmp(strrchr(arg, '.'), ".c++"))) {
      // printf(".cc/cpp/c++ file found");
      c_file_arr.push_back(argstr);
    } else if (!output_name.empty() && (argstr == output_name)) {
      // printf("checking output name\n");
      continue;
    } else if (!include_path.empty() && (argstr == include_path)) {
      include_path.clear();
      continue;
    } else if (strrchr(arg, '.') && strcmp(strrchr(arg, '.'), ".o") == 0) {
      //std::cout << ".o file found: " << argstr << endl;
      o_file_arr.push_back(argstr);
    } else if (strrchr(arg, '.') && strcmp(strrchr(arg, '.'), ".a") == 0) {
      // std::cout << ".a file found: " << argstr << endl;
      a_file_arr.push_back(argstr);
    } else if (!strcmp(arg, "-c")) {
      link = false;
      temp = false;
    } else if (argstr.find("-I") != string::npos) {
      compiler_flags.push_back(argstr);
      linker_flags.push_back(argstr);
      if (i != args.size() - 1 && args[i + 1].at(0) != '-') {
        include_path = args[i+1];
        compiler_flags.push_back(include_path);
        linker_flags.push_back(include_path);
      }
    } else if (std::any_of(supported_compile_flags.begin(), supported_compile_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      // checking compiler flags
      compiler_flags.push_back(argstr);
    } else if (std::any_of(supported_linker_flags.begin(), supported_linker_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      // checking linker flags
      linker_flags.push_back(argstr);
    } else if (std::any_of(supported_flags.begin(), supported_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      // checking supported flags
      compiler_flags.push_back(argstr);
      linker_flags.push_back(argstr);
    } else {
      // Call subprocess
      std::cout << "Unrecognized flag: " << arg << endl;
      args.erase(args.begin() + i);
      // args.erase(args.begin());
      vector<char*> vc;
      transform(args.begin(), args.end(), back_inserter(vc), convert);
      cout << compiler << " " << &vc[0] << endl;
      if (execvp(compiler, &vc[0]) == -1) std::cout << "unrecognized flag exec failed" << endl;
      return 1;
    }
  }
  string tmp;
  if (temp) {
    tmp = filesystem::temp_directory_path();
  }

  vector<pid_t> threads_arr;
  
  // Compile each .cc file
  // std::cout << "Compiling c files" << endl;
  for (string arg : c_file_arr) {
    // std::cout << "arg: " << arg.c_str() << endl;
    string o_file_str;
    if (temp) {
      // create temp .o file
      string tem = tmp + "/XXXXXX.o";

      char* writable_template = strdup(tem.c_str());
      int rc = mkstemps(writable_template, 2);
      if (rc == -1) {
        std::cout << "Could not create temporary file." << endl;
      }

      o_file_str = string(writable_template);
      free(writable_template);
    } else {
      auto suffix_pos = arg.rfind('.');
      o_file_str = arg.substr(0, suffix_pos) + ".o";
    }
    o_file_arr.push_back(o_file_str);
    // std::cout << "o_file named: " << o_file_str.c_str() << endl;
    vector<string> compile_args = {compiler_str, "-c", "-o", o_file_str, arg};

    compile_args.insert(compile_args.end(), compiler_flags.begin(), compiler_flags.end());

    vector<char*> thread_args;
    transform(compile_args.begin(), compile_args.end(), back_inserter(thread_args), convert);
    thread_args.push_back(NULL);
    
    if (!print) {
      pid_t cpid;
      if ((cpid = fork()) == 0) {
        // std::cout << compiler << " " << thread_args.data() << endl;
        if(execvp(compiler, thread_args.data()) == -1){
          std::cout << "Compile failed: " << arg << endl;
        }
        exit(1);
      }
      threads_arr.push_back(cpid);
    } else {
      string compile_args_str;
      for (string arg : compile_args) {
        compile_args_str += arg + " ";
      }
      std::cout << "Test Print: " << compile_args_str << endl;
    }
  }
  // std::cout << "Waiting for threads to join" << endl;

  for (int i = 0; i < threads_arr.size(); i++) {
    while (waitpid(threads_arr[i], NULL, 0) > 0)
      ;
  }

  // Link .o files.
  // std::cout << "Linking all files" << endl;
  if (link){
    vector<string> link_vec = {compiler, "-o", output_name};
    for (string o_file : o_file_arr) {
      link_vec.push_back(o_file);
    } 
    for (string a_file : a_file_arr) {
      link_vec.push_back(a_file);
    }
    for (string linker_flag : linker_flags) {
      link_vec.push_back(linker_flag);
    }
    vector<char*> link_args;
    transform(link_vec.begin(), link_vec.end(), back_inserter(link_args), convert);
    link_args.push_back(NULL);
    if (!print) {
      int stat;
      if(fork() == 0){
        // std::cout << "Linking started." << endl;
        if(execvp(compiler,  link_args.data()) == -1) {
          std::cout << "Linking failed." << endl;
        } else {
          // std::cout << "Done linking." << endl;
        }
        exit(1);
      } else {
        wait(&stat);
      }
      if (WIFEXITED(stat)) {
        //printf("Exit status: %d\n", WEXITSTATUS(stat));
      }
      else if (WIFSIGNALED(stat)){
        psignal(WTERMSIG(stat), "Exit signal");
      }
    } else {
      for (char* command : link_args) {
        std::cout << command << endl;
      }
    }
  }  
  // if (temp)
  //   for (string o_file : o_file_arr) unlink(o_file.c_str());
  // std::cout << "Build complete!" << endl;
  return 1;
}

int main(int argc, char* argv[]) {
  // std::cout << "Using our CLANG" << endl;
  // Remove wrappers from the PATH
  char* pathC = getenv("PATH");
  vector<string> path_arr;
  // todo: account for null path
  if (pathC != NULL) {
    string paths = string(pathC);
    // split path into vector using delimiter ':'
    string tmp;
    stringstream ss(paths);
    while (getline(ss, tmp, ':')) path_arr.push_back(tmp);
    for (vector<string>::iterator it = path_arr.begin(); it != path_arr.end();) {
      if ((*it).find("wrappers") != string::npos) {
        it = path_arr.erase(it);
      } else
        ++it;
    }
    paths.clear();

    for (const auto& path : path_arr) paths += path + ":";

    // std::cout << "new path: " << paths.c_str() << endl;
    setenv("PATH", paths.c_str(), 1);
  }

  vector<string> args(argv, argv + argc);
  return clang_wrapper(args);
}
