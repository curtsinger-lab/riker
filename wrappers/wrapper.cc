#include <algorithm>
#include <array>
#include <filesystem>
#include <iostream>
#include <list>
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

// Initialize Global variables
vector<string> subprocess_arr, c_file_arr, o_file_arr, a_file_arr, compiler_flags, linker_flags;
bool link_bool = true, temp = true, print = false;
string output_name, include_path, compiler_str;
const char* compiler;

// Helper function for convert
char* convert_helper(const string& s) {
  char* pc = new char[s.size() + 1];
  strcpy(pc, s.c_str());
  return pc;
}

// This function erase the first element of vec and return the new vector
vector<string> pop_front(vector<string> vec) {
  vec.erase(vec.begin());
  return vec;
}

// Remove the wrappers folder from the PATH environment variable
void path_update(char* pathC) {
  vector<string> path_arr;
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

  // combine vector of strings back into a single path string
  for (const auto& path : path_arr) paths += path + ":";
  setenv("PATH", paths.c_str(), 1);
}

// This function transforms the command arguments from a vector<string> to vector<char*> then calls
// execvp to execute the compile command.
int exec_fnc(vector<string> vs) {
  vector<char*> vc;
  transform(vs.begin(), vs.end(), back_inserter(vc), convert_helper);
  vc.push_back(NULL);
  return execvp(compiler_str.c_str(), vc.data());
}

// This function works though the argument command
void parse_args(vector<string> args) {
  // std::cout << "Start of Arg Parsing." << endl;

  if (!(!strcmp(compiler, "clang") || !strcmp(compiler, "gcc") || !strcmp(compiler, "cc") ||
        !strcmp(compiler, "clang++") || !strcmp(compiler, "g++") || !strcmp(compiler, "c++"))) {
    // If not using an accepted compiler
    std::cout << "Error - unrecognized compiler:" << compiler << endl;
    exit(0);
  }

  vector<string> supported_flags = {"-print", "-D", "-f",   "-o",    "-W",        "-pthread", "-g",
                                    "-M",     "-O", "-std", "--std", "-pedantic", "-m", "-U"};
  vector<string> supported_compile_flags = {};
  vector<string> supported_linker_flags = {"-L", "-shared", "-Wl", "-l", "-r"};

  // preserve a copy of the original command in case we need to call it
  vector<string> command = args;
  int i = 0;               // counter for removing the -nowrapper flag
  args = pop_front(args);  // take out the compiler
  while (args.size() > 0) {
    string argstr = args[0].data();
    i++;

    // std::cout << "arg: " << argstr << endl;
    if (argstr.rfind("-print", 0) == 0) {
      print = true;
      args = pop_front(args);
    } else if (argstr.rfind("-o", 0) == 0) {
      output_name = args[1].data();
      if (output_name.find(".o") != string::npos) {
        temp = false;
        link_bool = false;
        o_file_arr.push_back(output_name);
      }
      args = pop_front(args);
      args = pop_front(args);
    } else if ((argstr.size() > 2) && (0 == argstr.compare(argstr.size() - 2, 2, ".c"))) {
      c_file_arr.push_back(argstr);
      args = pop_front(args);
    } else if ((argstr.size() > 3) && ((0 == argstr.compare(argstr.size() - 3, 3, ".cc")))) {
      c_file_arr.push_back(argstr);
      args = pop_front(args);
    } else if ((argstr.size() > 4) && ((0 == argstr.compare(argstr.size() - 4, 4, ".cpp")) ||
                                       (0 == argstr.compare(argstr.size() - 4, 4, ".c++")))) {
      c_file_arr.push_back(argstr);
      args = pop_front(args);
    } else if ((argstr.size() > 2) && (0 == argstr.compare(argstr.size() - 2, 2, ".o"))) {
      // std::cout << ".o file found: " << argstr << endl;
      o_file_arr.push_back(argstr);
      args = pop_front(args);
    } else if ((argstr.size() > 2) && (0 == argstr.compare(argstr.size() - 2, 2, ".a"))) {
      // std::cout << ".a file found: " << argstr << endl;
      a_file_arr.push_back(argstr);
      args = pop_front(args);
    } else if (argstr == "-c") {
      link_bool = false;
      temp = false;
      args = pop_front(args);
    } else if (argstr.find("-I") != string::npos) {
      compiler_flags.push_back(argstr);
      linker_flags.push_back(argstr);
      if (args.size() > 1 && args[1].at(0) != '-') {
        include_path = args[1].data();
        compiler_flags.push_back(include_path);
        linker_flags.push_back(include_path);
        args = pop_front(args);
      }
      args = pop_front(args);
    } else if (std::any_of(supported_compile_flags.begin(), supported_compile_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      compiler_flags.push_back(argstr);
      args = pop_front(args);
    } else if (std::any_of(supported_linker_flags.begin(), supported_linker_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      linker_flags.push_back(argstr);
      args = pop_front(args);
    } else if (std::any_of(supported_flags.begin(), supported_flags.end(),
                           [&](string flag) { return argstr.rfind(flag, 0) == 0; })) {
      compiler_flags.push_back(argstr);
      linker_flags.push_back(argstr);
      args = pop_front(args);
    } else {
      // Call subprocess
      std::cout << "Unrecognized flag: " << argstr << endl;
      if (exec_fnc(command) == -1) {
        std::cout << "unrecognized flag exec failed" << endl;
      }
    }
  }
}

// This function do the compilation in multiple threads
void compile() {
  string tmp;
  if (temp) {
    tmp = filesystem::temp_directory_path();
  }

  vector<pid_t> threads_arr;

  // Compile each .cc file
  // std::cout << "Compiling c files" << endl;
  for (string arg : c_file_arr) {
    // std::cout << "arg: " << arg << endl;
    string o_file_str;
    if (temp) {
      // create temp .o file
      string tem = tmp + "/XXXXXX.o";

      char* writable_template = tem.data();
      int rc = mkstemps(writable_template, 2);
      if (rc == -1) {
        std::cout << "Could not create temporary file." << endl;
      }
      o_file_str = string(writable_template);

    } else {
      auto suffix_pos = arg.rfind('.');
      o_file_str = arg.substr(0, suffix_pos) + ".o";
    }

    o_file_arr.push_back(o_file_str);
    // std::cout << ".o file named: " << o_file_str << endl;

    vector<string> compile_args = {compiler_str, "-c", "-o", o_file_str, arg};
    compile_args.insert(compile_args.end(), compiler_flags.begin(), compiler_flags.end());

    if (!print) {
      pid_t cpid;
      if ((cpid = fork()) == 0) {
        if (exec_fnc(compile_args) == -1) {
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

  while (waitpid(-1, NULL, 0) > 0)
    ;

  // std::cout << "Finish waiting threads" << endl;
}

// This function link the compiled files in a single thread
void linking() {
  vector<string> link_vec = {compiler_str, "-o", output_name};
  for (string o_file : o_file_arr) {
    link_vec.push_back(o_file);
  }
  for (string a_file : a_file_arr) {
    link_vec.push_back(a_file);
  }
  for (string linker_flag : linker_flags) {
    link_vec.push_back(linker_flag);
  }
  if (!print) {
    int stat;
    if (fork() == 0) {
      if (exec_fnc(link_vec) == -1) {
        std::cout << "Linking failed." << endl;
      }
      exit(1);
    } else {
      wait(&stat);
    }
    if (WIFSIGNALED(stat)) {
      psignal(WTERMSIG(stat), "Exit signal");
      exit(0);
    }
  } else {
    for (string command : link_vec) {
      std::cout << command << endl;
    }
  }
}

// Main function that divides up the work
int clang_wrapper(vector<string> args) {
  // std::cout << "Start of clang wrapper" << endl;

  // Determine compiler
  compiler_str = args[0].data();
  compiler = compiler_str.c_str();
  const char* last = strrchr(compiler, '/');
  if (last != NULL) {
    compiler = last + 1;
  }

  // Work through the command
  parse_args(args);
  // std::cout << compiler << endl;

  compile();  // Compile .c files

  // Link .o files.
  if (link_bool) {
    linking();
  }

  return 1;
}

int main(int argc, char* argv[]) {
  std::cout << "Using our CLANG" << endl;

  // Remove wrappers from the PATH
  char* pathC = getenv("PATH");
  if (pathC != NULL) {
    path_update(pathC);
  }

  vector<string> args(argv, argv + argc);
  return clang_wrapper(args);
}
