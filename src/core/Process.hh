#pragma once

#include <map>
#include <set>
#include <string>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/File.hh"

struct FileDescriptor;

struct Process {
  pid_t thread_id;
  std::map<int, FileDescriptor> fds;
  std::set<File*> mmaps;

  Process(pid_t thread_id, std::string cwd, Command* command);

  Command* getCommand() { return _command; }

  void chdir(std::string newdir);

  void chroot(std::string newroot);

  Process* fork(pid_t child_pid);

  void exec(BuildGraph& trace, std::string exe_path);

 private:
  Command* _command;
  std::string _cwd;
  std::string _root;
};
