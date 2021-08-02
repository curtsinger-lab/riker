#pragma once

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <functional>
#include <memory>
#include <ostream>
#include <stack>
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
#include "tracing/inject.h"
#include "util/log.hh"

namespace fs = std::filesystem;

class AccessFlags;
class Build;
class Command;
class Tracer;

class Thread {
 public:
  Thread(Tracer& tracer, std::shared_ptr<Process> process, pid_t tid) noexcept :
      _tracer(tracer), _process(process), _tid(tid) {}

  /// Get the process this thread runs in
  std::shared_ptr<Process> getProcess() const noexcept { return _process; }

  /// Get the command this thread's process is running
  const std::shared_ptr<Command>& getCommand() const noexcept { return _process->getCommand(); }

  /// Get the thread ID
  pid_t getID() const noexcept { return _tid; }

  /// Traced entry to a system call through the provided shared memory channel
  void syscallEntryChannel(Build& build, ssize_t channel) noexcept;

  /// Traced exit from a system call through the provided shared memory channel
  void syscallExitChannel(Build& build, ssize_t channel) noexcept;

  /// Traced exit from a system call using ptrace
  void syscallExitPtrace(Build& build) noexcept;

  /// Check if a ptrace stop can be skipped because a shared memory channel is in use
  bool canSkipTrace(user_regs_struct& regs) const noexcept;

  /// Resume a traced thread that is currently stopped
  void resume() noexcept;

  /// Resume a thread that has stopped before a syscall, and run the provided handler when the
  /// syscall finishes
  void finishSyscall(std::function<void(Build&, long)> handler) noexcept;

  /// Force the tracee to exit with a given exit code. This currently only works on entry to an
  /// execve call (which is where we need it to implement skipping)
  void forceExit(int status) noexcept;

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
  Ref::ID makePathRef(Build& build,
                      fs::path p,
                      AccessFlags flags,
                      at_fd at = at_fd::cwd()) noexcept;

  /*** Handling for specific system calls ***/

  // File Opening, Creation, and Closing
  void _open(Build& build, fs::path f, o_flags flags, mode_flags mode) noexcept {
    _openat(build, at_fd::cwd(), f, flags, mode);
  }
  void _openat(Build& build, at_fd dfd, fs::path filename, o_flags flags, mode_flags mode) noexcept;
  void _creat(Build& build, fs::path p, mode_flags mode) noexcept {
    _open(build, p, o_flags(O_CREAT | O_WRONLY | O_TRUNC), mode);
  }
  void _mknod(Build& build, fs::path f, mode_flags mode, unsigned dev) noexcept {
    _mknodat(build, at_fd::cwd(), f, mode, dev);
  }
  void _mknodat(Build& build, at_fd dfd, fs::path filename, mode_flags mode, unsigned dev) noexcept;
  void _close(Build& build, int fd) noexcept;

  // Pipes
  void _pipe(Build& build, int* fds) noexcept { _pipe2(build, fds, o_flags()); }
  void _pipe2(Build& build, int* fds, o_flags flags) noexcept;

  // File Descriptor Manipulation
  void _dup(Build& build, int fd) noexcept;
  void _dup2(Build& build, int oldfd, int newfd) noexcept { _dup3(build, oldfd, newfd, o_flags()); }
  void _dup3(Build& build, int oldfd, int newfd, o_flags flags) noexcept;
  void _fcntl(Build& build, int fd, int cmd, unsigned long arg) noexcept;

  // Metadata Operations
  void _access(Build& build, fs::path pathname, int mode) noexcept {
    _faccessat(build, at_fd::cwd(), pathname, mode, 0);
  }
  void _faccessat(Build& build, at_fd dirfd, fs::path pathname, int mode, at_flags flags) noexcept;
  void _stat(Build& build, fs::path pathname, struct stat* statbuf) noexcept {
    _fstatat(build, at_fd::cwd(), pathname, statbuf, at_flags());
  }
  void _lstat(Build& build, fs::path pathname, struct stat* statbuf) noexcept {
    _fstatat(build, at_fd::cwd(), pathname, statbuf, at_flags(AT_SYMLINK_NOFOLLOW));
  }
  void _fstat(Build& build, int fd, struct stat* statbuf) noexcept {
    _fstatat(build, at_fd(fd), "", statbuf, at_flags(AT_EMPTY_PATH));
  }
  void _fstatat(Build& build,
                at_fd dirfd,
                fs::path pathname,
                struct stat* statbuf,
                at_flags flags) noexcept;
  void _newfstatat(Build& build,
                   at_fd dirfd,
                   fs::path pathname,
                   struct stat* statbuf,
                   at_flags flags) noexcept {
    _fstatat(build, dirfd, pathname, statbuf, flags);
  }
  void _statx(Build& build, at_fd dfd, fs::path pathname, at_flags flags) noexcept {
    _fstatat(build, dfd, pathname, nullptr, flags);
  }
  void _chown(Build& build, fs::path f, uid_t usr, gid_t grp) noexcept {
    _fchownat(build, at_fd::cwd(), f, usr, grp, at_flags(0));
  }
  void _lchown(Build& build, fs::path f, uid_t usr, gid_t grp) noexcept {
    _fchownat(build, at_fd::cwd(), f, usr, grp, at_flags(AT_SYMLINK_NOFOLLOW));
  }
  void _fchown(Build& build, int fd, uid_t user, gid_t group) noexcept;
  void _fchownat(Build& build,
                 at_fd dfd,
                 fs::path filename,
                 uid_t user,
                 gid_t group,
                 at_flags flag) noexcept;
  void _chmod(Build& build, fs::path filename, mode_flags mode) noexcept {
    _fchmodat(build, at_fd::cwd(), filename, mode, 0);
  }
  void _fchmod(Build& build, int fd, mode_flags mode) noexcept;
  void _fchmodat(Build& build,
                 at_fd dfd,
                 fs::path filename,
                 mode_flags mode,
                 at_flags flags) noexcept;

  // File Content Operations
  void _read(Build& build, int fd) noexcept;
  void _readv(Build& build, int fd) noexcept { _read(build, fd); }
  void _preadv(Build& build, int fd) noexcept { _read(build, fd); }
  void _preadv2(Build& build, int fd) noexcept { _read(build, fd); }
  void _pread64(Build& build, int fd) noexcept { _read(build, fd); }
  void _write(Build& build, int fd) noexcept;
  void _writev(Build& build, int fd) noexcept { _write(build, fd); }
  void _pwritev(Build& build, int fd) noexcept { _write(build, fd); }
  void _pwritev2(Build& build, int fd) noexcept { _write(build, fd); }
  void _pwrite64(Build& build, int fd) noexcept { _write(build, fd); }
  void _mmap(Build& build, void* addr, size_t len, int prot, int flags, int fd, off_t off) noexcept;
  void _truncate(Build& build, fs::path path, long length) noexcept;
  void _ftruncate(Build& build, int fd, long length) noexcept;
  void _tee(Build& build, int fd_in, int fd_out) noexcept;
  void _splice(Build& build, int in, loff_t off_in, int out, loff_t off_out) noexcept {
    _tee(build, in, out);
  }
  void _copy_file_range(Build& build, int fd_in, int _, int fd_out) noexcept {
    _tee(build, fd_in, fd_out);
  }
  void _sendfile(Build& build, int out_fd, int in_fd) noexcept { _tee(build, in_fd, out_fd); }
  void _vmsplice(Build& build, int fd) noexcept { _write(build, fd); }

  // Directory Operations
  void _mkdir(Build& build, fs::path p, mode_flags mode) noexcept {
    _mkdirat(build, at_fd::cwd(), p, mode);
  }
  void _mkdirat(Build& build, at_fd dfd, fs::path pathname, mode_flags mode) noexcept;
  void _rmdir(Build& build, fs::path pathname) noexcept {
    _unlinkat(build, at_fd::cwd(), pathname, at_flags(AT_REMOVEDIR));
  }
  void _rename(Build& build, fs::path n1, fs::path n2) noexcept {
    _renameat(build, at_fd::cwd(), n1, at_fd::cwd(), n2);
  }
  void _renameat(Build& build, at_fd d1, fs::path n1, at_fd d2, fs::path n2) noexcept {
    _renameat2(build, d1, n1, d2, n2, rename_flags());
  }
  void _renameat2(Build& build,
                  at_fd old_dfd,
                  fs::path oldpath,
                  at_fd new_dfd,
                  fs::path newpath,
                  rename_flags flags) noexcept;
  void _getdents(Build& build, int fd) noexcept;
  void _getdents64(Build& build, int fd) noexcept { _getdents(build, fd); }

  // Link and Symlink Operations
  void _link(Build& build, fs::path oldname, fs::path newname) {
    _linkat(build, at_fd::cwd(), oldname, at_fd::cwd(), newname, at_flags());
  }
  void _linkat(Build& build,
               at_fd old_dfd,
               fs::path oldpath,
               at_fd new_dfd,
               fs::path newpath,
               at_flags flags) noexcept;
  void _symlink(Build& build, fs::path oldname, fs::path newname) noexcept {
    _symlinkat(build, oldname, at_fd::cwd(), newname);
  }
  void _symlinkat(Build& build, fs::path oldname, at_fd newdfd, fs::path newname) noexcept;
  void _readlink(Build& build, fs::path path) noexcept { _readlinkat(build, at_fd::cwd(), path); }
  void _readlinkat(Build& build, at_fd dfd, fs::path pathname) noexcept;
  void _unlink(Build& build, fs::path pathname) noexcept {
    _unlinkat(build, at_fd::cwd(), pathname, 0);
  }
  void _unlinkat(Build& build, at_fd dfd, fs::path pathname, at_flags flags) noexcept;

  // Socket Operations
  void _recvfrom(Build& build,
                 int sockfd,
                 void* buf,
                 size_t len,
                 int flags,
                 struct sockaddr* src_addr,
                 socklen_t* addrlen) noexcept {
    FAIL << "recvfrom(2) not yet implemented.";
  }
  void _recvmsg(Build& build, int sockfd, struct msghdr* msg, int flags) noexcept {
    FAIL << "recvmsg(2) not yet implemented.";
  }
  void _sendmsg(Build& build, int sockfd, const struct msghdr* msg, int flags) {
    FAIL << "sendmsg(2) not yet implemented.";
  }
  void _sendto(Build& build,
               int sockfd,
               const void* buf,
               size_t len,
               int flags,
               const struct sockaddr* dest_addr,
               socklen_t addrlen) {
    FAIL << "sendto(2) not yet implemented.";
  }
  void _socket(Build& build, int domain, int type, int protocol) noexcept;
  void _socketpair(Build& build, int domain, int type, int protocol, int sv[2]) noexcept;

  // Process State Operations
  void _umask(Build& build, mode_t mask) noexcept;
  void _chdir(Build& build, fs::path filename) noexcept;
  void _chroot(Build& build, fs::path filename) noexcept;
  void _pivot_root(Build& build, fs::path new_root, fs::path put_old) noexcept;
  void _fchdir(Build& build, int fd) noexcept;
  void _execve(Build& build, fs::path filename, std::vector<std::string> args) noexcept {
    _execveat(build, at_fd::cwd(), filename, args);
  }
  void _execveat(Build& build,
                 at_fd dfd,
                 fs::path filename,
                 std::vector<std::string> args) noexcept;
  void _wait4(Build& build, pid_t pid, int* wstatus, int options) noexcept;
  void _waitid(Build& build, idtype_t idtype, id_t id, siginfo_t* infop, int options) noexcept;

  /// Print a thread to an output stream
  friend std::ostream& operator<<(std::ostream& o, const Thread& t) noexcept {
    return o << "[Thread " << t._tid << " in " << t._process << "]";
  }

  /// Print a thread pointer
  friend std::ostream& operator<<(std::ostream& o, const Thread* t) noexcept {
    if (t == nullptr) return o << "<null Thread>";
    return o << *t;
  }

  /********** Utility Templates for Running Syscall Handlers **********/

  /**
   * A simple wrapper class around system call arguments that can convert them to known types by
   * reading from a stopped thread.
   */
  class SyscallArgWrapper {
   public:
    SyscallArgWrapper(Thread* thread, unsigned long val) : _thread(thread), _val(val) {}

    // Use static cast for most types
    template <typename T>
    operator T() {
      return static_cast<T>(_val);
    }

    // Get an at_fd from the register value
    operator at_fd() { return at_fd(_val); }

    // Get o_flags from the register value
    operator o_flags() { return o_flags(_val); }

    // Get at_flags from the register value
    operator at_flags() { return at_flags(_val); }

    // Get mode_flags from the register value
    operator mode_flags() { return mode_flags(_val); }

    // Get rename_flags from the register value
    operator rename_flags() { return rename_flags(_val); }

    // Read a string from the thread's memory
    operator std::string() { return _thread->readString(_val); }

    // Get an fs::path from the thread's memory
    operator fs::path() { return _thread->readPath(_val); }

    // Read a vector of strings from the thread's memory
    operator std::vector<std::string>() { return _thread->readArgvArray(_val); }

    // Cast directly to pointer types
    template <typename T>
    operator T*() {
      return (T*)_val;
    }

   private:
    Thread* _thread;
    unsigned long _val;
  };

  SyscallArgWrapper wrap(unsigned long val) noexcept { return SyscallArgWrapper(this, val); }

  template <class Output>
  void invokeHandler(void (Thread::*handler)(Output&), Output& out, const user_regs_struct& regs) {
    (this->*(handler))(out);
  }

  template <class Output, class T1>
  void invokeHandler(void (Thread::*handler)(Output&, T1),
                     Output& out,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, wrap(regs.SYSCALL_ARG1));
  }

  template <class Output, class T1, class T2>
  void invokeHandler(void (Thread::*handler)(Output&, T1, T2),
                     Output& out,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2));
  }

  template <class Output, class T1, class T2, class T3>
  void invokeHandler(void (Thread::*handler)(Output&, T1, T2, T3),
                     Output& out,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2),
                       wrap(regs.SYSCALL_ARG3));
  }

  template <class Output, class T1, class T2, class T3, class T4>
  void invokeHandler(void (Thread::*handler)(Output&, T1, T2, T3, T4),
                     Output& out,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2),
                       wrap(regs.SYSCALL_ARG3), wrap(regs.SYSCALL_ARG4));
  }

  template <class Output, class T1, class T2, class T3, class T4, class T5>
  void invokeHandler(void (Thread::*handler)(Output&, T1, T2, T3, T4, T5),
                     Output& out,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2),
                       wrap(regs.SYSCALL_ARG3), wrap(regs.SYSCALL_ARG4), wrap(regs.SYSCALL_ARG5));
  }

  template <class Output, class T1, class T2, class T3, class T4, class T5, class T6>
  void invokeHandler(void (Thread::*handler)(Output&, T1, T2, T3, T4, T5, T6),
                     Output& out,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2),
                       wrap(regs.SYSCALL_ARG3), wrap(regs.SYSCALL_ARG4), wrap(regs.SYSCALL_ARG5),
                       wrap(regs.SYSCALL_ARG6));
  }

 private:
  /// The tracer that is executing this thread
  Tracer& _tracer;

  /// The process this thread is executing in
  std::shared_ptr<Process> _process;

  /// The thread's tid
  pid_t _tid;

  /// The stack of post-syscall handlers to invoke. System calls can nest when a signal is delivered
  /// during a blocked system call (e.g. SIGCHLD is sent to bash while it is reading)
  std::stack<std::function<void(Build&, long)>> _post_syscall_handlers;

  /// Which channel is this thread using for the current trace event? Set to -1 if not using one.
  ssize_t _channel = -1;
};
