#pragma once

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <functional>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>

#include "runtime/Ref.hh"
#include "tracing/Flags.hh"
#include "tracing/Process.hh"
#include "util/log.hh"

namespace fs = std::filesystem;

class AccessFlags;
class Build;
class Command;
class Tracer;

class Thread {
 public:
  Thread(Build& build, Tracer& tracer, std::shared_ptr<Process> process, pid_t tid) noexcept :
      _build(build), _tracer(tracer), _process(process), _tid(tid) {}

  /// Get the process this thread runs in
  std::shared_ptr<Process> getProcess() const noexcept { return _process; }

  /// Get the command this thread's process is running
  const std::shared_ptr<Command>& getCommand() const noexcept { return _process->getCommand(); }

  /// Get the thread ID
  pid_t getID() const noexcept { return _tid; }

  /// Resume a traced thread that is currently stopped
  void resume() noexcept;

  /// Resume a thread that has stopped before a syscall, and run the provided handler when the
  /// syscall finishes
  void finishSyscall(std::function<void(long)> handler) noexcept;

  /// Run the registered post-syscall handler
  void syscallFinished() noexcept;

  /// Get the special event message attached to some ptrace stops (clone, fork, etc.)
  unsigned long getEventMessage() noexcept;

  /// Get the current register state for this thread
  user_regs_struct getRegisters() noexcept;

  /// Change the register state for this thread
  void setRegisters(user_regs_struct& regs) noexcept;

  /// Read a string from this thread's memory
  std::string readString(uintptr_t tracee_pointer) noexcept;

  /// Read a normalized path from this thread's memory
  fs::path readPath(uintptr_t tracee_pointer) noexcept;

  /// Read a value from this thread's memory
  template <typename T = uintptr_t>
  T readData(uintptr_t tracee_pointer) noexcept;

  /// Read a terminated array from this thread's memory
  template <typename T, T Terminator, size_t BatchSize = 128>
  std::vector<T> readTerminatedArray(uintptr_t tracee_pointer) noexcept;

  /// Read a null-terminated array of strings
  std::vector<std::string> readArgvArray(uintptr_t tracee_pointer) noexcept;

  /// Get the path associated with a file descriptor that may be AT_FDCWD
  fs::path getPath(at_fd fd) const noexcept;

  /**
   * This thread referenced a path. Create an Access reference to track this reference and record it
   * in the command.
   * \param p     The path, as retrieved from the trace (MUST NOT BE NORMALIZED)
   * \param flags The flags that control the access mode
   * \param at    A file descriptor this access is made relative to
   * \returns an Access instance that has been added to the current command
   */
  Ref::ID makePathRef(fs::path p, AccessFlags flags, at_fd at = at_fd::cwd()) noexcept;

  /*** Handling for specific system calls ***/

  // File Opening, Creation, and Closing
  void _open(fs::path f, o_flags flags, mode_flags mode) noexcept {
    _openat(at_fd::cwd(), f, flags, mode);
  }
  void _openat(at_fd dfd, fs::path filename, o_flags flags, mode_flags mode) noexcept;
  void _creat(fs::path p, mode_flags mode) noexcept {
    _open(p, o_flags(O_CREAT | O_WRONLY | O_TRUNC), mode);
  }
  void _mknod(fs::path f, mode_flags mode, unsigned dev) noexcept {
    _mknodat(at_fd::cwd(), f, mode, dev);
  }
  void _mknodat(at_fd dfd, fs::path filename, mode_flags mode, unsigned dev) noexcept;
  void _close(int fd) noexcept;

  // Pipes
  void _pipe(int* fds) noexcept { _pipe2(fds, o_flags()); }
  void _pipe2(int* fds, o_flags flags) noexcept;

  // File Descriptor Manipulation
  void _dup(int fd) noexcept;
  void _dup2(int oldfd, int newfd) noexcept { _dup3(oldfd, newfd, o_flags()); }
  void _dup3(int oldfd, int newfd, o_flags flags) noexcept;
  void _fcntl(int fd, int cmd, unsigned long arg) noexcept;

  // Metadata Operations
  void _access(fs::path pathname, int mode) noexcept {
    _faccessat(at_fd::cwd(), pathname, mode, 0);
  }
  void _faccessat(at_fd dirfd, fs::path pathname, int mode, at_flags flags) noexcept;
  void _stat(fs::path pathname, struct stat* statbuf) noexcept {
    _fstatat(at_fd::cwd(), pathname, statbuf, at_flags());
  }
  void _lstat(fs::path pathname, struct stat* statbuf) noexcept {
    _fstatat(at_fd::cwd(), pathname, statbuf, at_flags(AT_SYMLINK_NOFOLLOW));
  }
  void _fstat(int fd, struct stat* statbuf) noexcept {
    _fstatat(at_fd(fd), "", statbuf, at_flags(AT_EMPTY_PATH));
  }
  void _fstatat(at_fd dirfd, fs::path pathname, struct stat* statbuf, at_flags flags) noexcept;
  void _newfstatat(at_fd dirfd, fs::path pathname, struct stat* statbuf, at_flags flags) noexcept {
    _fstatat(dirfd, pathname, statbuf, flags);
  }
  void _statx(at_fd dfd, fs::path pathname, at_flags flags) noexcept {
    _fstatat(dfd, pathname, nullptr, flags);
  }
  void _chown(fs::path f, uid_t usr, gid_t grp) noexcept {
    _fchownat(at_fd::cwd(), f, usr, grp, at_flags(0));
  }
  void _lchown(fs::path f, uid_t usr, gid_t grp) noexcept {
    _fchownat(at_fd::cwd(), f, usr, grp, at_flags(AT_SYMLINK_NOFOLLOW));
  }
  void _fchown(int fd, uid_t user, gid_t group) noexcept;
  void _fchownat(at_fd dfd, fs::path filename, uid_t user, gid_t group, at_flags flag) noexcept;
  void _chmod(fs::path filename, mode_flags mode) noexcept {
    _fchmodat(at_fd::cwd(), filename, mode, 0);
  }
  void _fchmod(int fd, mode_flags mode) noexcept;
  void _fchmodat(at_fd dfd, fs::path filename, mode_flags mode, at_flags flags) noexcept;

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
  void _truncate(fs::path path, long length) noexcept;
  void _ftruncate(int fd, long length) noexcept;
  void _tee(int fd_in, int fd_out) noexcept;
  void _splice(int in, loff_t off_in, int out, loff_t off_out) noexcept { _tee(in, out); }
  void _copy_file_range(int fd_in, int _, int fd_out) noexcept { _tee(fd_in, fd_out); }
  void _sendfile(int out_fd, int in_fd) noexcept { _tee(in_fd, out_fd); }
  void _vmsplice(int fd) noexcept { _write(fd); }

  // Directory Operations
  void _mkdir(fs::path p, mode_flags mode) noexcept { _mkdirat(at_fd::cwd(), p, mode); }
  void _mkdirat(at_fd dfd, fs::path pathname, mode_flags mode) noexcept;
  void _rmdir(fs::path pathname) noexcept {
    _unlinkat(at_fd::cwd(), pathname, at_flags(AT_REMOVEDIR));
  }
  void _rename(fs::path n1, fs::path n2) noexcept { _renameat(at_fd::cwd(), n1, at_fd::cwd(), n2); }
  void _renameat(at_fd d1, fs::path n1, at_fd d2, fs::path n2) noexcept {
    _renameat2(d1, n1, d2, n2, rename_flags());
  }
  void _renameat2(at_fd old_dfd,
                  fs::path oldpath,
                  at_fd new_dfd,
                  fs::path newpath,
                  rename_flags flags) noexcept;
  void _getdents(int fd) noexcept;
  void _getdents64(int fd) noexcept { _getdents(fd); }

  // Link and Symlink Operations
  void _link(fs::path oldname, fs::path newname) {
    _linkat(at_fd::cwd(), oldname, at_fd::cwd(), newname, at_flags());
  }
  void _linkat(at_fd old_dfd,
               fs::path oldpath,
               at_fd new_dfd,
               fs::path newpath,
               at_flags flags) noexcept;
  void _symlink(fs::path oldname, fs::path newname) noexcept {
    _symlinkat(oldname, at_fd::cwd(), newname);
  }
  void _symlinkat(fs::path oldname, at_fd newdfd, fs::path newname) noexcept;
  void _readlink(fs::path path) noexcept { _readlinkat(at_fd::cwd(), path); }
  void _readlinkat(at_fd dfd, fs::path pathname) noexcept;
  void _unlink(fs::path pathname) noexcept { _unlinkat(at_fd::cwd(), pathname, 0); }
  void _unlinkat(at_fd dfd, fs::path pathname, at_flags flags) noexcept;

  // Socket Operations
  void _accept(int sockfd, struct sockaddr* addr, socklen_t* addrlen) noexcept {
    FAIL << "accept(2) not yet implemented.";
  }
  void _bind(int sockfd, const struct sockaddr* addr, socklen_t addrlen) noexcept;
  void _connect(int sockfd, const struct sockaddr* addr, socklen_t addrlen) noexcept {
    FAIL << "connect(2) not yet implemented.";
  }
  void _getpeername(int sockfd, struct sockaddr* addr, socklen_t* addrlen) noexcept {
    FAIL << "getpeername(2) not yet implemented.";
  }
  void _getsockname(int sockfd, struct sockaddr* addr, socklen_t* addrlen) noexcept {
    FAIL << "getsockname(2) not yet implemented.";
  }
  void _getsockopt(int sockfd, int level, int optname, void* optval, socklen_t* optlen) noexcept {
    FAIL << "getsockopt(2) not yet implemented.";
  }
  void _listen(int sockfd, int backlog) { FAIL << "listen(2) not yet implemented."; }
  void _recvfrom(int sockfd,
                 void* buf,
                 size_t len,
                 int flags,
                 struct sockaddr* src_addr,
                 socklen_t* addrlen) noexcept {
    FAIL << "recvfrom(2) not yet implemented.";
  }
  void _recvmsg(int sockfd, struct msghdr* msg, int flags) noexcept {
    FAIL << "recvmsg(2) not yet implemented.";
  }
  void _sendmsg(int sockfd, const struct msghdr* msg, int flags) {
    FAIL << "sendmsg(2) not yet implemented.";
  }
  void _sendto(int sockfd,
               const void* buf,
               size_t len,
               int flags,
               const struct sockaddr* dest_addr,
               socklen_t addrlen) {
    FAIL << "sendto(2) not yet implemented.";
  }
  void _setsockopt(int sockfd,
                   int level,
                   int optname,
                   const void* optval,
                   socklen_t optlen) noexcept {
    FAIL << "setsockopt(2) not yet implemented.";
  }
  void _shutdown(int sockfd, int how) { FAIL << "shutdown(2) not yet implemented."; }
  void _socket(int domain, int type, int protocol) noexcept;
  void _socketpair(int domain, int type, int protocol, int sv[2]) noexcept;

  // Process State Operations
  void _chdir(fs::path filename) noexcept;
  void _chroot(fs::path filename) noexcept;
  void _pivot_root(fs::path new_root, fs::path put_old) noexcept;
  void _fchdir(int fd) noexcept;
  void _fork() noexcept;
  void _vfork() noexcept { _fork(); }
  void _clone(void* fn, void* stack, int flags) noexcept;
  void _exit(int status) noexcept;
  void _exit_group(int status) noexcept;
  void _execve(fs::path filename,
               std::vector<std::string> args,
               std::vector<std::string> env) noexcept {
    _execveat(at_fd::cwd(), filename, args, env);
  }
  void _execveat(at_fd dfd,
                 fs::path filename,
                 std::vector<std::string> args,
                 std::vector<std::string> env) noexcept;
  void _wait4(pid_t pid, int* wstatus, int options) noexcept;
  void _waitid(idtype_t idtype, id_t id, siginfo_t* infop, int options) noexcept;

  /// Print a thread to an output stream
  friend std::ostream& operator<<(std::ostream& o, const Thread& t) noexcept {
    return o << "[Thread " << t._tid << " in " << t._process << "]";
  }

  /// Print a thread pointer
  friend std::ostream& operator<<(std::ostream& o, const Thread* t) noexcept {
    if (t == nullptr) return o << "<null Thread>";
    return o << *t;
  }

 private:
  /// This thread is running as a part of a build execution
  Build& _build;

  /// The tracer that is executing this thread
  Tracer& _tracer;

  /// The process this thread is executing in
  std::shared_ptr<Process> _process;

  /// The thread's tid
  pid_t _tid;

  /// The handler function that should run when the next system call is finished
  std::function<void(long)> _post_syscall_handler;
};
