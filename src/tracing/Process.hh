#pragma once

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <map>
#include <memory>
#include <ostream>
#include <set>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/user.h>

#include "tracing/FDEntry.hh"

using std::map;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

namespace fs = std::filesystem;

class Build;
class Command;
class Rebuild;

class Process {
 public:
  Process(Build& build, pid_t pid, fs::path cwd, shared_ptr<Command> command,
          map<int, FDEntry> fds) :
      _build(build), _pid(pid), _cwd(cwd), _command(command), _fds(fds) {}

  /// Resume a traced process that is currently stopped
  void resume();

  /// Resume a traced process so it can execute a system call, then stop it and return
  long finishSyscall();

  /// Get the special event message attached to some ptrace stops (clone, fork, etc.)
  unsigned long getEventMessage();

  /// Get the current register state for this process
  user_regs_struct getRegisters();

  /// Read a string from this process' memory
  string readString(uintptr_t tracee_pointer);

  /// Read a value from this process' memory
  template <typename T = uintptr_t>
  T readData(uintptr_t tracee_pointer);

  /// Read a terminated array from this process' memory
  template <typename T, T Terminator, size_t BatchSize = 128>
  vector<T> readTerminatedArray(uintptr_t tracee_pointer);

  /// Read a null-terminated array of strings
  vector<string> readArgvArray(uintptr_t tracee_pointer);

  /// Resolve and normalize a path
  /// Returns an absolute path
  fs::path resolvePath(fs::path p, int at = AT_FDCWD);

  /// Print a process to an output stream
  friend ostream& operator<<(ostream& o, const Process& p) {
    o << p._pid << ": " << p._command << "\n";
    for (auto& e : p._fds) {
      o << "  " << e.first << ": " << e.second << "\n";
    }
    return o;
  }

  /// Print a process pointer
  friend ostream& operator<<(ostream& o, const Process* p) { return o << *p; }

  /*** Handling for specific system calls ***/
  void _read(int fd);
  void _write(int fd);
  void _close(int fd);
  void _mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off);
  int _dup(int fd);
  void _sendfile(int out_fd, int in_fd);
  void _execveat(int dfd, string filename, vector<string> args, vector<string> env);
  void _fcntl(int fd, int cmd, unsigned long arg);
  void _truncate(string path, long length);
  void _ftruncate(int fd, long length);
  void _chdir(string filename);
  void _fchdir(int fd);
  void _lchown(string filename, uid_t user, gid_t group);
  void _chroot(string filename);
  void _setxattr(string pathname);
  void _lsetxattr(string pathname);
  void _getxattr(string pathname);
  void _lgetxattr(string pathname);
  void _openat(int dfd, string filename, int flags, mode_t mode);
  void _mkdirat(int dfd, string pathname, mode_t mode);
  void _mknodat(int dfd, string filename, mode_t mode, unsigned dev);
  void _fchownat(int dfd, string filename, uid_t user, gid_t group, int flag);
  void _unlinkat(int dfd, string pathname, int flag);
  void _symlinkat(string oldname, int newdfd, string newname);
  void _readlinkat(int dfd, string pathname);
  void _fchmodat(int dfd, string filename, mode_t mode, int flags);
  void _tee(int fd_in, int fd_out);
  void _dup3(int oldfd, int newfd, int flags);
  void _pipe2(int* fds, int flags);
  void _renameat2(int old_dfd, string oldpath, int new_dfd, string newpath, int flags);
  void _faccessat(int dirfd, string pathname, int mode, int flags);
  void _fstatat(int dirfd, string pathname, int flags);
  void _lseek(int fd, off_t offset, int whence);

  /*** Syscalls that should be handled directly, but are currently aliases ***/
  void _rmdir(string p) { _unlink(p); }
  void _fchmod(int fd, mode_t mode) { _write(fd); }
  void _fchown(int fd, uid_t user, gid_t group) { _write(fd); }
  void _fsetxattr(int fd) { _write(fd); }
  void _fgetxattr(int fd) { _read(fd); }
  void _flistxattr(int fd) { _read(fd); }
  void _fremovexattr(int fd) { _write(fd); }
  void _vmsplice(int fd) { _write(fd); }

  /*** Syscalls that can be handled as aliases for others ***/
  void _open(string f, int flags, mode_t mode) { _openat(AT_FDCWD, f, flags, mode); }
  void _pread64(int fd) { _read(fd); }
  void _pwrite64(int fd) { _write(fd); }
  void _readv(int fd) { _read(fd); }
  void _writev(int fd) { _write(fd); }
  void _pipe(int* fds) { _pipe2(fds, 0); }
  void _dup2(int oldfd, int newfd) { _dup3(oldfd, newfd, 0); }
  void _execve(string filename, vector<string> args, vector<string> env) {
    _execveat(AT_FDCWD, filename, args, env);
  }
  void _getdents(int fd) { _read(fd); }
  void _rename(string n1, string n2) { _renameat(AT_FDCWD, n1, AT_FDCWD, n2); }
  void _mkdir(string p, mode_t mode) { _mkdirat(AT_FDCWD, p, mode); }
  void _creat(string p, mode_t mode) { _open(p, O_CREAT | O_WRONLY | O_TRUNC, mode); }
  void _unlink(string pathname) { _unlinkat(AT_FDCWD, pathname, 0); }
  void _symlink(string oldname, string newname) { _symlinkat(oldname, AT_FDCWD, newname); }
  void _readlink(string path) { _readlinkat(AT_FDCWD, path); }
  void _chmod(string filename, mode_t mode) { _fchmodat(AT_FDCWD, filename, mode, 0); }
  void _chown(string f, uid_t user, gid_t group) { _fchownat(AT_FDCWD, f, user, group, 0); }
  void _mknod(string f, mode_t mode, unsigned dev) { _mknodat(AT_FDCWD, f, mode, dev); }
  void _listxattr(string pathname) { _getxattr(pathname); }
  void _llistxattr(string pathname) { _lgetxattr(pathname); }
  void _removexattr(string pathname) { _setxattr(pathname); }
  void _lremovexattr(string pathname) { _lsetxattr(pathname); }
  void _getdents64(int fd) { _read(fd); }
  void _renameat(int d1, string n1, int d2, string n2) { _renameat2(d1, n1, d2, n2, 0); }
  void _splice(int fd_in, loff_t off_in, int fd_out, loff_t off_out) { _tee(fd_in, fd_out); }
  void _copy_file_range(int fd_in, int _, int fd_out) { _tee(fd_in, fd_out); }
  void _preadv2(int fd) { _read(fd); }
  void _pwritev2(int fd) { _write(fd); }
  void _access(string pathname, int mode) { _faccessat(AT_FDCWD, pathname, mode, 0); }
  void _preadv(int fd) { _read(fd); }
  void _pwritev(int fd) { _write(fd); }
  void _stat(string pathname) { _fstatat(AT_FDCWD, pathname, 0); }
  void _fstat(int fd) { _fstatat(fd, "", AT_EMPTY_PATH); }
  void _lstat(string pathname) { _fstatat(AT_FDCWD, pathname, AT_SYMLINK_NOFOLLOW); }

  Build& _build;
  pid_t _pid;
  string _cwd;
  string _root;
  shared_ptr<Command> _command;
  set<shared_ptr<Artifact>> _mmaps;
  map<int, FDEntry> _fds;
};
