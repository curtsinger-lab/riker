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

char* convert(const string& s) {
  char* pc = new char[s.size() + 1];
  strcpy(pc, s.c_str());
  return pc;
}

int clang_wrapper(vector<string> args) {
  // printf("Start of clang wrapper\n");
  std::cout << "Start of clang wrapper" << endl;
  vector<string> supported_flags = {"-print", "-D", "-f",   "-o",    "-W",        "-pthread", "-g",
                                    "-M",     "-O", "-std", "--std", "-pedantic", "-m"};
  vector<string> supported_compile_flags = {};
  vector<string> supported_linker_flags = {"-L", "-shared", "-Wl", "-l", "-r", "../deps/"};

  // Determine compiler and whether we are using C++
  string compiler_str = args.front();
  const char* compiler = compiler_str.c_str();
  const char* last = strrchr(compiler, '/');
  if (last != NULL) {
    compiler = last + 1;
  }
  // printf("Compiler: %s\n", compiler);
  std::cout << "Compiler: " << compiler << endl;
  bool is_cpp = false;
  if (!strcmp(compiler, "clang") || !strcmp(compiler, "gcc") || !strcmp(compiler, "cc")) {
    is_cpp = false;
  } else if (!strcmp(compiler, "clang++") || !strcmp(compiler, "g++") || !strcmp(compiler, "c++")) {
    is_cpp = true;
  } else {
    // printf("Error - unrecognized compiler: %s\n", compiler);
    std::cout << "Error - unrecognized compiler:" << compiler << endl;
    return 1;
  }

  // Initialize arrays
  vector<string> subprocess_arr, c_file_arr, o_file_arr, compiler_flags, linker_flags;
  bool link = true, temp = true, print = false;
  string output_name, include_path;

  // Work through the command
  for (int i = 1; i < args.size(); i++) {
    // printf("i: %d\n", i);
    string argstr = args[i];
    const char* arg = argstr.c_str();
    if (!strcmp(arg, "-nowrapper")) {
      // Call subprocess
      args.erase(args.begin() + i);
      args.erase(args.begin());
      vector<char*> vc;
      transform(args.begin(), args.end(), back_inserter(vc), convert);
      execvp(compiler, &vc[0]);
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
    } else if (!output_name.empty() && (arg == output_name)) {
      // printf("checking output name\n");
      continue;
    } else if (!include_path.empty() && (argstr == include_path)) {
      continue;
    } else if (strrchr(arg, '.') && strcmp(strrchr(arg, '.'), ".o") == 0) {
      // printf("checking .o\n");
      o_file_arr.push_back(argstr);
    } else if (!strcmp(arg, "-c")) {
      // printf("checking -c\n");
      link = false;
    } else if (argstr.find("-I") != string::npos) {
      compiler_flags.push_back(argstr);
      linker_flags.push_back(argstr);
      if (i != args.size() - 1 && args[i + 1].at(0) != '-') {
        include_path = args[i + 1];
        compiler_flags.push_back(include_path);
        linker_flags.push_back(include_path);
      }
    } else if (std::any_of(supported_compile_flags.begin(), supported_compile_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      // printf("checking compiler flags\n");
      compiler_flags.push_back(argstr);
    } else if (std::any_of(supported_linker_flags.begin(), supported_linker_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      // printf("checking linker flags\n");
      linker_flags.push_back(argstr);
    } else if (std::any_of(supported_flags.begin(), supported_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      // printf("checking supported flags\n");
      compiler_flags.push_back(argstr);
      linker_flags.push_back(argstr);
    } else {
      // Call subprocess
      // printf("Unrecognized flag: %s\n", arg);
      std::cout << "Unrecognized flag: " << arg << endl;
      args.erase(args.begin() + i);
      args.erase(args.begin());
      vector<char*> vc;
      transform(args.begin(), args.end(), back_inserter(vc), convert);
      execvp(compiler, &vc[0]);
      return 1;
    }
  }
  string tmp;
  if (temp) {
    tmp = filesystem::temp_directory_path();
  }
  // std::vector<std::thread> threads_arr;
  vector<pid_t> threads_arr;
  // int status; // for waiting on threads
  // Compile each .cc file
  // printf("Compiling c files\n");
  std::cout << "Compiling c files" << endl;
  for (string arg : c_file_arr) {
    // printf("arg: %s\n", arg.c_str());
    std::cout << "arg: " << arg.c_str() << endl;
    // const char * o_file = arg.c_str();
    string o_file_str;
    if (temp) {
      // create temp .o file
      // printf("Creating a temp file\n");
      string tem = tmp + "/XXXXXX.o";

      char* writable_template = strdup(tem.c_str());
      int rc = mkstemps(writable_template, 2);
      if (rc == -1) {
        // printf("Could not create temporary file.\n");
        std::cout << "Could not create temporary file." << endl;
      }

      o_file_str = string(writable_template);
      free(writable_template);

      // vector<char> o_file_template(tem.begin(), tem.end());
      // o_file_template.push_back('\0');
      // std::copy(tem.begin(), tem.end(), o_file_template);
      // o_file_template[tem.size()] = '\0';
      // int rc = mkstemps(&o_file_template[0], 2);
      // if (rc == -1){
      //    printf("Could not create temporary file.\n");
      //}
      // printf("o_file_str: %s\n", o_file_str.c_str());
      // printf("o_file_template: %s\n", o_file_template);
      // strcpy(o_file_str, string(o_file_template));
      // o_file_str = &o_file_template[0];
      // printf("o_file_str: %s\n", o_file_str);
    } else {
      // const char * period_index = strrchr(o_file, '.');
      // o_file_str = arg;
      // o_file_str.replace(period_index-o_file+1, o_file_str.length()-(period_index-o_file), ".o");

      auto suffix_pos = arg.rfind('.');
      o_file_str = arg.substr(0, suffix_pos) + ".o";
    }
    o_file_arr.push_back(o_file_str);
    // printf("o_file created: %s\n", o_file_str.c_str());
    std::cout << "o_file created: " << o_file_str.c_str() << endl;
    vector<string> compile_args = {compiler_str, "-c", "-o", o_file_str, arg};
    // string compile_args_str;
    // for(string arg: compile_args){
    //     compile_args_str += arg + " ";
    // }
    // for(string arg: compiler_flags){
    //     compile_args_str += arg + " ";
    // }

    compile_args.insert(compile_args.end(), compiler_flags.begin(), compiler_flags.end());

    // std::for_each(compile_args.begin(), compile_args.end(), string::c_str);
    // compile_args.push_back(NULL);
    // char * thread_args = &compile_args[0];
    // vector<char*> thread_args(compile_args.size(),nullptr);
    // for (int i=0; i<compile_args.size();i++) {
    //     thread_args[i] = compile_args[i].c_str();
    // }
    vector<char*> thread_args;
    transform(compile_args.begin(), compile_args.end(), back_inserter(thread_args), convert);
    // The compile_args vector has to be a vector of char*s, not C++ strings
    // Make sure there is a NULL at the end of the compile_args vector
    // execv(compiler_str, compile_args.data())

    // printf("Compiling in new thread\n");
    if (!print) {
      // argument of type "const char **" is incompatible with parameter of type "char *const *"
      // std::thread new_thread([](char* const* to_run){execv(to_run[0], to_run);},
      // &thread_args[0]);
      pid_t cpid;
      if ((cpid = fork()) == 0) {
        // char*const to_run[] = &thread_args[0];
        // argument of type "const char **" is incompatible with parameter of type "char *const
        // *"C/C++(167)
        execv(compiler, &thread_args[0]);
        exit(1);
      }
      threads_arr.push_back(cpid);
    } else {
      string compile_args_str;
      for (string arg : compile_args) {
        compile_args_str += arg + " ";
      }
      // printf("TEST PRINT: %s\n", compile_args_str.c_str());
      std::cout << "Test Print: " << compile_args_str << endl;
    }
  }
  // printf("Waiting for threads to join\n");
  std::cout << "Waiting for threads to join" << endl;

  for (int i = 0; i < threads_arr.size(); i++) {
    while (waitpid(threads_arr[i], NULL, 0) > 0)
      ;
  }
  /*if (!print) {
      for (std::thread & subprocess_thread : threads_arr) {
          subprocess_thread.join();
      }
  }*/
  // Link .o files.
  // printf("Linking all files\n");
  std::cout << "Linking all files" << endl;
  if (link) {
    vector<string> link_vec = {compiler, "-o", output_name};
    for (string o_file : o_file_arr) {
      link_vec.push_back(o_file);
    }
    for (string linker_flag : linker_flags) {
      link_vec.push_back(linker_flag);
    }
    if (!print) {
      int stat;
      pid_t pid;
      vector<char*> link_args;
      transform(link_vec.begin(), link_vec.end(), back_inserter(link_args), convert);
      if ((pid = fork()) == 0) {
        std::cout << "start linking" << endl;
        execv(compiler, &link_args[0]);
        std::cout << "done linking" << endl;
        exit(1);
      } else {
        waitpid(pid, &stat, 0);
      }
      if (WIFEXITED(stat)) {
        // printf("Exit status: %d\n", WEXITSTATUS(stat));
      } else if (WIFSIGNALED(stat)) {
        psignal(WTERMSIG(stat), "Exit signal");
      }
      // system(&linkstr[0]);
    } else {
      string linkstr = compiler_str + " -o " + output_name;
      for (string o_file : o_file_arr) {
        linkstr += (" " + o_file);
      }
      for (string linker_flag : linker_flags) {
        linkstr += (" " + linker_flag);
      }
      // printf("TEST PRINT: %s\n", &linkstr[0]);
      std::cout << "TEST PRINT:" << &linkstr[0] << endl;
    }
  }
  for (string o_file : o_file_arr) unlink(o_file.c_str());
  std::cout << "Build complete!" << endl;
  return 1;
}

int main(int argc, char* argv[]) {
  // printf("Using our CLANG\n");
  std::cout << "Using our CLANG" << endl;
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

    // printf("new path: %s\n", paths.c_str());
    std::cout << "new path: " << paths.c_str() << endl;
    setenv("PATH", paths.c_str(), 1);
  }

  // int n = sizeof(argv) / sizeof(argv[0]);
  vector<string> args(argv, argv + argc);
  return clang_wrapper(args);
}
