#pragma once

#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>
#include <vector>

#include <sys/types.h>

#include "runtime/Command.hh"
#include "runtime/Ref.hh"

class Build;

class Process : public std::enable_shared_from_this<Process> {
 public:
  /// Keep track of file descriptors with a reference, and a boolean to track whether or not the
  /// descriptor is closed on an exec syscall
  using FileDescriptor = std::tuple<Ref::ID, bool>;

  Process(Build& build,
          std::shared_ptr<Command> command,
          pid_t pid,
          Ref::ID cwd,
          Ref::ID root,
          std::map<int, FileDescriptor> fds,
          std::optional<mode_t> umask = std::nullopt) noexcept;

  /// Get the process ID
  pid_t getID() const noexcept { return _pid; }

  /// Get the command this process is running
  const std::shared_ptr<Command>& getCommand() const noexcept { return _command; }

  /// Get the root directory
  Ref::ID getRoot() const noexcept { return _root; }

  /// Get the working directory
  Ref::ID getWorkingDir() const noexcept { return _cwd; }

  /// Set the working directory
  void setWorkingDir(Ref::ID ref) noexcept;

  /// Get the reference used for a given file descriptor entry
  Ref::ID getFD(int fd) noexcept;

  /// Check if this process has a particular file descriptor
  bool hasFD(int fd) const noexcept { return _fds.find(fd) != _fds.end(); }

  /// Add a file descriptor entry
  void addFD(int fd, Ref::ID ref, bool cloexec = false) noexcept;

  /// Remove a file descriptor entry
  void closeFD(int fd) noexcept;

  /// Remove a file descriptor entry if it exists. Return true if the close succeeded.
  bool tryCloseFD(int fd) noexcept;

  /// Set a file descriptor's close-on-exec flag
  void setCloexec(int fd, bool cloexec) noexcept;

  /// Mark this process as the primary process for its command
  void setPrimary() noexcept { _primary = true; }

  /// Has this process exited?
  bool hasExited() const noexcept { return _exited; }

  /// Set the process umask
  void setUmask(mode_t mask) noexcept { _umask = mask; }

  /// Get the process umask
  mode_t getUmask() const noexcept { return _umask; }

  /// This process forked off a child process
  std::shared_ptr<Process> fork(pid_t child_pid) noexcept;

  /// This process is executing a new file
  const std::shared_ptr<Command>& exec(Ref::ID exe_ref, std::vector<std::string> args) noexcept;

  /// This process is exiting
  void exit(int exit_status) noexcept;

  /// Set a callback that can be used to force this process to exit
  void waitForExit(std::function<void(int)> handler) noexcept;

  /// Force this traced process to exit. This is used to trigger an actual process exit in response
  /// to an emulated command that has been skipped.
  void forceExit(int exit_status) noexcept;

  /// Print a process to an output stream
  friend std::ostream& operator<<(std::ostream& o, const Process& p) noexcept {
    return o << "[Process " << p._pid << " (" << p._command->getShortName() << ")]";
  }

  /// Print a process pointer
  friend std::ostream& operator<<(std::ostream& o, const Process* p) noexcept {
    if (p == nullptr) return o << "<null Process>";
    return o << *p;
  }

 private:
  /// This process is running as part of a build
  Build& _build;

  /// The command this process is running
  std::shared_ptr<Command> _command;

  /// Is this process the primary process running its command?
  bool _primary = false;

  /// The process' pid
  pid_t _pid;

  /// A reference to the process' current working directory
  Ref::ID _cwd;

  /// A reference to the process' current root directory
  Ref::ID _root;

  /// The current umask for the process
  mode_t _umask;

  /// The process' file descriptor table
  std::map<int, FileDescriptor> _fds;

  /// Has this process exited?
  bool _exited = false;

  /// The callback to force this process to exit
  std::function<void(int)> _force_exit_callback;
};
