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

class Process {
 public:
  /****** Constructors ******/
  Process(pid_t pid, std::string cwd, std::shared_ptr<Command> command) :
      _pid(pid),
      _command(command),
      _cwd(cwd) {}

  // Disallow Copy
  Process(const Process&) = delete;
  Process& operator=(const Process&) = delete;

  // Allow Move
  Process(Process&&) = default;
  Process& operator=(Process&&) = default;

  /****** Non-trivial methods ******/

  void setDefaultFds(std::shared_ptr<File> stdin, std::shared_ptr<File> stdout,
                     std::shared_ptr<File> stderr);

  void traceMmap(int fd);

  void traceChdir(std::string newdir);

  void traceChroot(std::string newroot);

  void traceClose(int fd);

  std::shared_ptr<Process> traceFork(pid_t child_pid);

  void traceOpen(int fd, std::shared_ptr<File> f, int flags, mode_t mode);

  void traceRead(std::shared_ptr<File> f);

  void traceRead(int fd);

  void traceModify(std::shared_ptr<File> f);

  void traceModify(int fd);

  void traceCreate(std::shared_ptr<File> f);

  void traceCreate(int fd);

  void traceRemove(std::shared_ptr<File> f);

  void traceRemove(int fd);

  void tracePipe(int fd1, int fd2, std::shared_ptr<File> f, bool cloexec);

  void traceDup(int fd, int new_fd, bool cloexec);

  void traceSetCloexec(int fd, bool cloexec);

  void traceExec(Tracer& tracer, BuildGraph& graph, std::string executable,
                 const std::list<std::string>& args);

  void traceExit();

  /****** Getters and setters ******/

  std::shared_ptr<Command> getCommand() const { return _command; }

 private:
  pid_t _pid;
  std::shared_ptr<Command> _command;
  std::string _cwd;
  std::string _root;
  std::set<std::shared_ptr<File>> _mmaps;
  std::map<int, FileDescriptor> _fds;
};
