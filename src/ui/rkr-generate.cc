#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>
#include <vector>

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

using std::cout;
using std::endl;
using std::fstream;
using std::string;
using std::vector;

// Helper function for convert
char* convert_helper(const string& s) {
  char* pc = new char[s.size() + 1];
  strcpy(pc, s.c_str());
  return pc;
}

bool hasEnding(string const& fullString, string const& ending) {
  if (fullString.length() >= ending.length()) {
    return (0 ==
            fullString.compare(fullString.length() - ending.length(), ending.length(), ending));
  } else {
    return false;
  }
}

void removeNonCommands(string file_name) {
  vector<string> lines_to_remove;
  fstream original_file(file_name.c_str(), fstream::in | fstream::out);
  fstream new_file("temp.txt", fstream::trunc | fstream::out);
  string line;
  string delimiter = " ";
  string token;
  while (getline(original_file, line)) {
    token = line.substr(0, line.find(delimiter));
    if (!hasEnding(token, ":")) {
      // string line_copy(line);
      // lines_to_remove.push_back(line_copy);
      new_file << line << endl;
    }
  }
  remove(file_name.c_str());
  rename("temp.txt", file_name.c_str());
}

int main() {
  string rikerfile_name = "Rikerfile";
  fstream write_file(rikerfile_name.c_str(), fstream::trunc | fstream::out);
  write_file << "#!/bin/sh" << endl << endl;
  write_file.close();
  string command = "make --always-make >> " + rikerfile_name;
  pclose(popen(command.c_str(), "w"));
  // getMakeCommands(&write_file, rikerfile_name);
  removeNonCommands(rikerfile_name);
  // fstream read_file(rikerfile_name.c_str(), fstream::in);
  // string output;
  // while (getline(read_file, output))
  // {
  //   cout << output << endl;
  // }
  // read_file.close();
  vector<string> vs = {"chmod", "u+x", rikerfile_name};
  vector<char*> vc;
  transform(vs.begin(), vs.end(), back_inserter(vc), convert_helper);
  vc.push_back(NULL);
  execvp("chmod", vc.data());
}