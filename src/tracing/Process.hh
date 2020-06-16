#pragma once

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <functional>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>

#include "core/FileDescriptor.hh"
#include "core/IR.hh"

using std::function;
using std::make_shared;
using std::map;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

namespace fs = std::filesystem;

class Build;
class Command;
class Tracer;

class Process {
 public:
  Process(Build& build, Tracer& tracer, shared_ptr<Command> command, pid_t pid,
          shared_ptr<Access> cwd, shared_ptr<Access> root, map<int, FileDescriptor> fds) noexcept :
      _build(build),
      _tracer(tracer),
      _command(command),
      _pid(pid),
      _cwd(cwd),
      _root(root),
      _fds(fds) {}

  shared_ptr<Process> fork(pid_t child_pid) noexcept {
    return make_shared<Process>(_build, _tracer, _command, child_pid, _cwd, _root, _fds);
  }

  /// Has this process exited?
  bool hasExited() const noexcept { return _exited; }

  void setExited() noexcept { _exited = true; }

  /// Resume a traced process that is currently stopped
  void resume() noexcept;

  /// Resume a process that has stopped before a syscall, and run the provided handler when the
  /// syscall finishes
  void finishSyscall(function<void(long)> handler) noexcept;

  /// Run the registered post-syscall handler
  void syscallFinished() noexcept;

  /// Get the special event message attached to some ptrace stops (clone, fork, etc.)
  unsigned long getEventMessage() noexcept;

  /// Get the current register state for this process
  user_regs_struct getRegisters() noexcept;

  /// Read a string from this process' memory
  string readString(uintptr_t tracee_pointer) noexcept;

  /// Read a value from this process' memory
  template <typename T = uintptr_t>
  T readData(uintptr_t tracee_pointer) noexcept;

  /// Read a terminated array from this process' memory
  template <typename T, T Terminator, size_t BatchSize = 128>
  vector<T> readTerminatedArray(uintptr_t tracee_pointer) noexcept;

  /// Read a null-terminated array of strings
  vector<string> readArgvArray(uintptr_t tracee_pointer) noexcept;

  /**
   * The command running in this process referenced a path. Create an Access reference to track
   * this reference and record it in the command.
   * \param p     The path, as retrieved from the trace (MUST NOT BE NORMALIZED)
   * \param flags The flags that control the access mode
   * \param at    A file descriptor this access is made relative to
   * \returns an Access instance that has been added to the current command
   */
  shared_ptr<Access> makeAccess(fs::path p, AccessFlags flags, int at = AT_FDCWD) noexcept;

  /// Print a process to an output stream
  friend ostream& operator<<(ostream& o, const Process& p) noexcept {
    o << p._pid << ": " << p._command << "\n";
    for (const auto& [index, descriptor] : p._fds) {
      o << "  " << index << ": " << descriptor << "\n";
    }
    return o;
  }

  /// Print a process pointer
  friend ostream& operator<<(ostream& o, const Process* p) noexcept { return o << *p; }

  /*** Handling for specific system calls ***/

  // File Opening, Creation, and Closing
  void _open(string f, int flags, mode_t mode) noexcept { _openat(AT_FDCWD, f, flags, mode); }
  void _openat(int dfd, string filename, int flags, mode_t mode) noexcept;
  void _creat(string p, mode_t mode) noexcept { _open(p, O_CREAT | O_WRONLY | O_TRUNC, mode); }
  void _mknod(string f, mode_t mode, unsigned dev) noexcept { _mknodat(AT_FDCWD, f, mode, dev); }
  void _mknodat(int dfd, string filename, mode_t mode, unsigned dev) noexcept;
  void _close(int fd) noexcept;

  // Pipes
  void _pipe(int* fds) noexcept { _pipe2(fds, 0); }
  void _pipe2(int* fds, int flags) noexcept;

  // File Descriptor Manipulation
  void _dup(int fd) noexcept;
  void _dup2(int oldfd, int newfd) noexcept { _dup3(oldfd, newfd, 0); }
  void _dup3(int oldfd, int newfd, int flags) noexcept;
  void _fcntl(int fd, int cmd, unsigned long arg) noexcept;
  void _tee(int fd_in, int fd_out) noexcept;
  void _splice(int in, loff_t off_in, int out, loff_t off_out) noexcept { _tee(in, out); }
  void _copy_file_range(int fd_in, int _, int fd_out) noexcept { _tee(fd_in, fd_out); }

  // Metadata Operations
  void _access(string pathname, int mode) noexcept { _faccessat(AT_FDCWD, pathname, mode, 0); }
  void _faccessat(int dirfd, string pathname, int mode, int flags) noexcept;
  void _stat(string pathname) noexcept { _fstatat(AT_FDCWD, pathname, 0); }
  void _lstat(string pathname) noexcept { _fstatat(AT_FDCWD, pathname, AT_SYMLINK_NOFOLLOW); }
  void _fstat(int fd) noexcept { _fstatat(fd, "", AT_EMPTY_PATH); }
  void _fstatat(int dirfd, string pathname, int flags) noexcept;
  void _statx(int dfd, string pathname, int flags) noexcept { _fstatat(dfd, pathname, flags); }
  void _chown(string f, uid_t usr, gid_t grp) noexcept { _fchownat(AT_FDCWD, f, usr, grp, 0); }
  void _lchown(string filename, uid_t user, gid_t group) noexcept;
  void _fchown(int fd, uid_t user, gid_t group) noexcept;
  void _fchownat(int dfd, string filename, uid_t user, gid_t group, int flag) noexcept;
  void _chmod(string filename, mode_t mode) noexcept { _fchmodat(AT_FDCWD, filename, mode, 0); }
  void _fchmod(int fd, mode_t mode) noexcept;
  void _fchmodat(int dfd, string filename, mode_t mode, int flags) noexcept;

  // File Content Operations
  void _read(int fd) noexcept;
  void _readv(int fd) noexcept { _read(fd); }
  void _preadv(int fd) noexcept { _read(fd); }
  void _preadv2(int fd) noexcept { _read(fd); }
  void _pread64(int fd) noexcept { _read(fd); }
  void _write(int fd) noexcept;
  void _writev(int fd) noexcept { _write(fd); }
  void _pwritev(int fd) noexcept { _write(fd); }
  void _pwritev2(int fd) noexcept { _write(fd); }
  void _pwrite64(int fd) noexcept { _write(fd); }
  void _mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off) noexcept;
  void _sendfile(int out_fd, int in_fd) noexcept;
  void _truncate(string path, long length) noexcept;
  void _ftruncate(int fd, long length) noexcept;
  void _vmsplice(int fd) noexcept;

  // Directory Operations
  void _mkdir(string p, mode_t mode) noexcept { _mkdirat(AT_FDCWD, p, mode); }
  void _mkdirat(int dfd, string pathname, mode_t mode) noexcept;
  void _rmdir(string p) noexcept;
  void _rename(string n1, string n2) noexcept { _renameat(AT_FDCWD, n1, AT_FDCWD, n2); }
  void _renameat(int d1, string n1, int d2, string n2) noexcept { _renameat2(d1, n1, d2, n2, 0); }
  void _renameat2(int old_dfd, string oldpath, int new_dfd, string newpath, int flags) noexcept;
  void _getdents(int fd) noexcept;
  void _getdents64(int fd) noexcept { _getdents(fd); }

  // Link and Symlink Operations
  void _symlink(string oldname, string newname) noexcept { _symlinkat(oldname, AT_FDCWD, newname); }
  void _symlinkat(string oldname, int newdfd, string newname) noexcept;
  void _readlink(string path) noexcept { _readlinkat(AT_FDCWD, path); }
  void _readlinkat(int dfd, string pathname) noexcept;
  void _unlink(string pathname) noexcept { _unlinkat(AT_FDCWD, pathname, 0); }
  void _unlinkat(int dfd, string pathname, int flag) noexcept;

  // Process State Operations
  void _chdir(string filename) noexcept;
  void _chroot(string filename) noexcept;
  void _pivot_root(string new_root, string put_old) noexcept;
  void _fchdir(int fd) noexcept;
  void _execve(string filename, vector<string> args, vector<string> env) noexcept {
    _execveat(AT_FDCWD, filename, args, env);
  }
  void _execveat(int dfd, string filename, vector<string> args, vector<string> env) noexcept;
  void _wait4(pid_t pid, int* wstatus, int options) noexcept;
  void _waitid(idtype_t idtype, id_t id, siginfo_t* infop, int options) noexcept;

  /// The build this process is running as a part of
  Build& _build;

  /// The tracer tha manages this process
  Tracer& _tracer;

  /// The command this process is running
  shared_ptr<Command> _command;

  /// Has this process exited?
  bool _exited = false;

  pid_t _pid;                     //< This process' PID
  shared_ptr<Access> _cwd;        //< A reference to this process' current working directory
  shared_ptr<Access> _root;       //< A reference to this process' current root directory
  map<int, FileDescriptor> _fds;  //< This process' file descriptor table

  /// The handler function that should run when the next system call is finished
  function<void(long)> _post_syscall_handler;
};
