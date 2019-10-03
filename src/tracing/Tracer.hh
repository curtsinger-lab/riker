#pragma once

#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>

#include <sys/types.h>

#include "core/FileDescriptor.hh"

class BuildGraph;
class Command;
class Process;

using std::list;
using std::map;
using std::set;
using std::shared_ptr;
using std::string;

struct file_reference {
  // fd may be AT_FDCWD and path may be NULL, but not both. If both are present,
  // then path is relative with respect to the directory in fd.
  int fd;
  std::string path;
  // Whether to follow the link if the reference points at a symlink
  bool follow_links;
};

class Tracer {
 public:
  Tracer(BuildGraph& graph) : _graph(graph) {}

  void run(shared_ptr<Command> cmd);

  void newProcess(pid_t pid, shared_ptr<Command> cmd);

  /****** Trace handler methods ******/

  void traceRead(pid_t pid, struct file_reference& file);

  void traceModify(pid_t pid, struct file_reference& file);

  void traceCreate(pid_t pid, struct file_reference& file);

  void traceRemove(pid_t pid, struct file_reference& file);

  void traceChdir(pid_t pid, string path);

  void traceChroot(pid_t pid, string path);

  void traceOpen(pid_t pid, int fd, string path, int flags, mode_t mode);

  void tracePipe(pid_t pid, int fds[2], bool cloexec);

  void traceDup(pid_t pid, int duped_fd, int new_fd, bool cloexec);

  void traceSetCloexec(pid_t pid, int fd, bool cloexec);

  void traceMmap(pid_t pid, int fd);

  void traceClose(pid_t pid, int fd);

  void traceFork(pid_t pid, pid_t child_pid);

  void traceClone(pid_t pid, pid_t thread_id);

  void traceExec(pid_t pid, string executable, const list<string>& args);

  void traceExit(pid_t pid);

 private:
  struct Process {
    Process(pid_t pid, string cwd, shared_ptr<Command> command, map<int, FileDescriptor> fds = {}) :
        _pid(pid),
        _command(command),
        _cwd(cwd),
        _fds(fds) {}

    pid_t _pid;
    shared_ptr<Command> _command;
    string _cwd;
    string _root;
    set<shared_ptr<File>> _mmaps;
    map<int, FileDescriptor> _fds;
  };

 private:
  BuildGraph& _graph;
  map<pid_t, shared_ptr<Process>> _processes;
};
