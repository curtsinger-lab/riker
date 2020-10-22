#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include "data/FileDescriptor.hh"
#include "runtime/Command.hh"

using std::make_shared;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::vector;

class Build;
class Ref;
class Tracer;

class Process : public std::enable_shared_from_this<Process> {
 public:
  Process(Build& build,
          Tracer& tracer,
          shared_ptr<Command> command,
          pid_t pid,
          shared_ptr<Ref> cwd,
          shared_ptr<Ref> root,
          map<int, FileDescriptor> fds) noexcept;

  /// Get the process ID
  pid_t getID() const noexcept { return _pid; }

  /// Get the command this process is running
  shared_ptr<Command> getCommand() const noexcept { return _command; }

  /// Get the root directory
  shared_ptr<Ref> getRoot() const noexcept { return _root; }

  /// Get the working directory
  shared_ptr<Ref> getWorkingDir() const noexcept { return _cwd; }

  /// Set the working directory
  void setWorkingDir(shared_ptr<Ref> ref) noexcept;

  /// Get a file descriptor entry
  FileDescriptor& getFD(int fd) noexcept;

  /// Check if this process has a particular file descriptor
  bool hasFD(int fd) const noexcept { return _fds.find(fd) != _fds.end(); }

  /// Add a file descriptor entry
  FileDescriptor& addFD(int fd,
                        shared_ptr<Ref> ref,
                        AccessFlags flags,
                        bool cloexec = false) noexcept;

  /// Remove a file descriptor entry
  void closeFD(int fd) noexcept;

  /// Remove a file descriptor entry if it exists
  void tryCloseFD(int fd) noexcept;

  /// Has this process exited?
  bool hasExited() const noexcept { return _exited; }

  /// This process forked off a child process
  shared_ptr<Process> fork(pid_t child_pid) noexcept;

  /// This process is executing a new file
  void exec(shared_ptr<Ref> exe_ref, vector<string> args, vector<string> env) noexcept;

  /// This process is exiting
  void exit() noexcept;

  /// Print a process to an output stream
  friend ostream& operator<<(ostream& o, const Process& p) noexcept {
    return o << "[Process " << p._pid << " (" << p._command->getShortName() << ")]";
  }

  /// Print a process pointer
  friend ostream& operator<<(ostream& o, const Process* p) noexcept {
    if (p == nullptr) return o << "<null Process>";
    return o << *p;
  }

 private:
  /// This process is running as part of a build
  Build& _build;

  /// This process was launched under a specific tracer
  Tracer& _tracer;

  /// The command this process is running
  shared_ptr<Command> _command;

  /// The process' pid
  pid_t _pid;

  /// A reference to the process' current working directory
  shared_ptr<Ref> _cwd;

  /// A reference to the process' current root directory
  shared_ptr<Ref> _root;

  /// The process' file descriptor table
  map<int, FileDescriptor> _fds;

  /// Has this process exited?
  bool _exited = false;
};
