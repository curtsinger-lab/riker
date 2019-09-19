#pragma once

#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include <sys/types.h>

#include "core/FileDescriptor.hh"

struct BuildGraph;
struct Command;
struct File;

struct Process : public std::enable_shared_from_this<Process> {
  pid_t thread_id;
  std::map<int, FileDescriptor> fds;
  std::set<File*> mmaps;

  Process(pid_t thread_id, std::string cwd, Command* command);

  Command* getCommand() { return _command; }

  void chdir(std::string newdir);

  void chroot(std::string newroot);

  std::shared_ptr<Process> fork(pid_t child_pid);

  void exec(BuildGraph& trace, std::string exe_path, const std::list<std::string>& args);

 private:
  Command* _command;
  std::string _cwd;
  std::string _root;
};
