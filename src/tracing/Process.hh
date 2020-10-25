#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <tuple>
#include <vector>

#include "runtime/Command.hh"

using std::make_shared;
using std::map;
using std::ostream;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

class Build;
class Ref;
class Tracer;

class Process : public std::enable_shared_from_this<Process> {
 public:
  /// Keep track of file descriptors with a reference, and a boolean to track whether or not the
  /// descriptor is closed on an exec syscall
  using FileDescriptor = tuple<Command::RefID, bool>;

  Process(Build& build,
          shared_ptr<Command> command,
          pid_t pid,
          Command::RefID cwd,
          Command::RefID root,
          map<int, FileDescriptor> fds) noexcept;

  /// Get the process ID
  pid_t getID() const noexcept { return _pid; }

  /// Get the command this process is running
  const shared_ptr<Command>& getCommand() const noexcept { return _command; }

  /// Get the root directory
  Command::RefID getRoot() const noexcept { return _root; }

  /// Get the working directory
  Command::RefID getWorkingDir() const noexcept { return _cwd; }

  /// Set the working directory
  void setWorkingDir(Command::RefID ref) noexcept;

  /// Get the reference used for a given file descriptor entry
  Command::RefID getFD(int fd) noexcept;

  /// Check if this process has a particular file descriptor
  bool hasFD(int fd) const noexcept { return _fds.find(fd) != _fds.end(); }

  /// Add a file descriptor entry
  void addFD(int fd, Command::RefID ref, bool cloexec = false) noexcept;

  /// Remove a file descriptor entry
  void closeFD(int fd) noexcept;

  /// Remove a file descriptor entry if it exists
  void tryCloseFD(int fd) noexcept;

  /// Set a file descriptor's close-on-exec flag
  void setCloexec(int fd, bool cloexec) noexcept;

  /// Has this process exited?
  bool hasExited() const noexcept { return _exited; }

  /// Get this process' exit status
  int getExitStatus() const noexcept { return _exit_status; }

  /// This process forked off a child process
  shared_ptr<Process> fork(pid_t child_pid) noexcept;

  /// This process is executing a new file
  void exec(Command::RefID exe_ref, vector<string> args, vector<string> env) noexcept;

  /// This process is exiting
  void exit(int exit_status) noexcept;

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

  /// The command this process is running
  shared_ptr<Command> _command;

  /// The process' pid
  pid_t _pid;

  /// A reference to the process' current working directory
  Command::RefID _cwd;

  /// A reference to the process' current root directory
  Command::RefID _root;

  /// The process' file descriptor table
  map<int, FileDescriptor> _fds;

  /// Has this process exited?
  bool _exited = false;

  /// What status did this process exit with?
  int _exit_status = -1;
};
