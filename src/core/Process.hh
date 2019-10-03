#pragma once

#include <cstdio>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include <sys/types.h>

#include "core/FileDescriptor.hh"

class BuildGraph;
class Command;
class File;
class Tracer;

using std::map;
using std::shared_ptr;
using std::string;

class Process {
 public:
  /****** Constructors ******/
  Process(pid_t pid, string cwd, shared_ptr<Command> command, map<int, FileDescriptor> fds = {}) :
      _pid(pid),
      _command(command),
      _cwd(cwd),
      _fds(fds) {}

  // Disallow Copy
  Process(const Process&) = delete;
  Process& operator=(const Process&) = delete;

  // Allow Move
  Process(Process&&) = default;
  Process& operator=(Process&&) = default;

  /****** Non-trivial methods ******/

  void traceMmap(int fd);

  void tracePipe(int fd1, int fd2, std::shared_ptr<File> f, bool cloexec);

  void traceDup(int fd, int new_fd, bool cloexec);

  void traceExec(Tracer& tracer, BuildGraph& graph, std::string executable,
                 const std::list<std::string>& args);

  /****** Getters and setters ******/

  std::shared_ptr<Command> getCommand() const { return _command; }

 public:
  pid_t _pid;
  std::shared_ptr<Command> _command;
  std::string _cwd;
  std::string _root;
  std::set<std::shared_ptr<File>> _mmaps;
  std::map<int, FileDescriptor> _fds;
};
