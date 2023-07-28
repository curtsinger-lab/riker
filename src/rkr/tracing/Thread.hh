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

#include "data/IRSource.hh"
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
  void syscallEntryChannel(Build& build, const IRSource& source, ssize_t channel) noexcept;

  /// Traced exit from a system call through the provided shared memory channel
  void syscallExitChannel(Build& build, const IRSource& source, ssize_t channel) noexcept;

  /// Traced exit from a system call using ptrace
  void syscallExitPtrace(Build& build, const IRSource& source) noexcept;

  /// Traced an exec
  void execPtrace(Build& build, const IRSource& source) noexcept;

  /// Check if a ptrace stop can be skipped because a shared memory channel is in use
  bool canSkipTrace(user_regs_struct& regs) const noexcept;

  /// Skip a system cal and return the given result instead
  void skip(int64_t result) noexcept;

  /// Resume a traced thread that is currently stopped
  void resume() noexcept;

  /// Resume a thread that has stopped before a syscall, and run the provided handler when the
  /// syscall finishes
  void finishSyscall(std::function<void(Build&, const IRSource&, long)> handler) noexcept;

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
                      const IRSource& source,
                      fs::path p,
                      AccessFlags flags,
                      at_fd at = at_fd::cwd()) noexcept;

  /*** Handling for specific system calls ***/

  // File Opening, Creation, and Closing
  void _open(Build& build,
             const IRSource& source,
             fs::path f,
             o_flags flags,
             mode_flags mode) noexcept {
    _openat(build, source, at_fd::cwd(), f, flags, mode);
  }
  void _openat(Build& build,
               const IRSource& source,
               at_fd dfd,
               fs::path filename,
               o_flags flags,
               mode_flags mode) noexcept;
  void _creat(Build& build, const IRSource& source, fs::path p, mode_flags mode) noexcept {
    _open(build, source, p, o_flags(O_CREAT | O_WRONLY | O_TRUNC), mode);
  }
  void _mknod(Build& build,
              const IRSource& source,
              fs::path f,
              mode_flags mode,
              unsigned dev) noexcept {
    _mknodat(build, source, at_fd::cwd(), f, mode, dev);
  }
  void _mknodat(Build& build,
                const IRSource& source,
                at_fd dfd,
                fs::path filename,
                mode_flags mode,
                unsigned dev) noexcept;
  void _close(Build& build, const IRSource& source, int fd) noexcept;

  // Pipes
  void _pipe(Build& build, const IRSource& source, int* fds) noexcept {
    _pipe2(build, source, fds, o_flags());
  }
  void _pipe2(Build& build, const IRSource& source, int* fds, o_flags flags) noexcept;

  // File Descriptor Manipulation
  void _dup(Build& build, const IRSource& source, int fd) noexcept;
  void _dup2(Build& build, const IRSource& source, int oldfd, int newfd) noexcept {
    _dup3(build, source, oldfd, newfd, o_flags());
  }
  void _dup3(Build& build, const IRSource& source, int oldfd, int newfd, o_flags flags) noexcept;
  void _fcntl(Build& build, const IRSource& source, int fd, int cmd, unsigned long arg) noexcept;

  // Metadata Operations
  void _access(Build& build, const IRSource& source, fs::path pathname, int mode) noexcept {
    _faccessat(build, source, at_fd::cwd(), pathname, mode, 0);
  }
  void _faccessat(Build& build,
                  const IRSource& source,
                  at_fd dirfd,
                  fs::path pathname,
                  int mode,
                  at_flags flags) noexcept;
  void _stat(Build& build,
             const IRSource& source,
             fs::path pathname,
             struct stat* statbuf) noexcept {
    _fstatat(build, source, at_fd::cwd(), pathname, statbuf, at_flags());
  }
  void _lstat(Build& build,
              const IRSource& source,
              fs::path pathname,
              struct stat* statbuf) noexcept {
    _fstatat(build, source, at_fd::cwd(), pathname, statbuf, at_flags(AT_SYMLINK_NOFOLLOW));
  }
  void _fstat(Build& build, const IRSource& source, int fd, struct stat* statbuf) noexcept {
    _fstatat(build, source, at_fd(fd), "", statbuf, at_flags(AT_EMPTY_PATH));
  }
  void _fstatat(Build& build,
                const IRSource& source,
                at_fd dirfd,
                fs::path pathname,
                struct stat* statbuf,
                at_flags flags) noexcept;
  void _newfstatat(Build& build,
                   const IRSource& source,
                   at_fd dirfd,
                   fs::path pathname,
                   struct stat* statbuf,
                   at_flags flags) noexcept {
    _fstatat(build, source, dirfd, pathname, statbuf, flags);
  }
  void _statx(Build& build,
              const IRSource& source,
              at_fd dfd,
              fs::path pathname,
              at_flags flags) noexcept {
    _fstatat(build, source, dfd, pathname, nullptr, flags);
  }
  void _chown(Build& build, const IRSource& source, fs::path f, uid_t usr, gid_t grp) noexcept {
    _fchownat(build, source, at_fd::cwd(), f, usr, grp, at_flags(0));
  }
  void _lchown(Build& build, const IRSource& source, fs::path f, uid_t usr, gid_t grp) noexcept {
    _fchownat(build, source, at_fd::cwd(), f, usr, grp, at_flags(AT_SYMLINK_NOFOLLOW));
  }
  void _fchown(Build& build, const IRSource& source, int fd, uid_t user, gid_t group) noexcept;
  void _fchownat(Build& build,
                 const IRSource& source,
                 at_fd dfd,
                 fs::path filename,
                 uid_t user,
                 gid_t group,
                 at_flags flag) noexcept;
  void _chmod(Build& build, const IRSource& source, fs::path filename, mode_flags mode) noexcept {
    _fchmodat(build, source, at_fd::cwd(), filename, mode, 0);
  }
  void _fchmod(Build& build, const IRSource& source, int fd, mode_flags mode) noexcept;
  void _fchmodat(Build& build,
                 const IRSource& source,
                 at_fd dfd,
                 fs::path filename,
                 mode_flags mode,
                 at_flags flags) noexcept;

  // File Content Operations
  void _read(Build& build, const IRSource& source, int fd) noexcept;
  void _readv(Build& build, const IRSource& source, int fd) noexcept { _read(build, source, fd); }
  void _preadv(Build& build, const IRSource& source, int fd) noexcept { _read(build, source, fd); }
  void _preadv2(Build& build, const IRSource& source, int fd) noexcept { _read(build, source, fd); }
  void _pread64(Build& build, const IRSource& source, int fd) noexcept { _read(build, source, fd); }
  void _write(Build& build, const IRSource& source, int fd) noexcept;
  void _writev(Build& build, const IRSource& source, int fd) noexcept { _write(build, source, fd); }
  void _pwritev(Build& build, const IRSource& source, int fd) noexcept {
    _write(build, source, fd);
  }
  void _pwritev2(Build& build, const IRSource& source, int fd) noexcept {
    _write(build, source, fd);
  }
  void _pwrite64(Build& build, const IRSource& source, int fd) noexcept {
    _write(build, source, fd);
  }
  void _mmap(Build& build,
             const IRSource& source,
             void* addr,
             size_t len,
             int prot,
             int flags,
             int fd,
             off_t off) noexcept;
  void _truncate(Build& build, const IRSource& source, fs::path path, long length) noexcept;
  void _ftruncate(Build& build, const IRSource& source, int fd, long length) noexcept;
  void _tee(Build& build, const IRSource& source, int fd_in, int fd_out) noexcept;
  void _splice(Build& build,
               const IRSource& source,
               int in,
               loff_t off_in,
               int out,
               loff_t off_out) noexcept {
    _tee(build, source, in, out);
  }
  void _copy_file_range(Build& build,
                        const IRSource& source,
                        int fd_in,
                        int _,
                        int fd_out) noexcept {
    _tee(build, source, fd_in, fd_out);
  }
  void _sendfile(Build& build, const IRSource& source, int out_fd, int in_fd) noexcept {
    _tee(build, source, in_fd, out_fd);
  }
  void _vmsplice(Build& build, const IRSource& source, int fd) noexcept {
    _write(build, source, fd);
  }

  // Directory Operations
  void _mkdir(Build& build, const IRSource& source, fs::path p, mode_flags mode) noexcept {
    _mkdirat(build, source, at_fd::cwd(), p, mode);
  }
  void _mkdirat(Build& build,
                const IRSource& source,
                at_fd dfd,
                fs::path pathname,
                mode_flags mode) noexcept;
  void _rmdir(Build& build, const IRSource& source, fs::path pathname) noexcept {
    _unlinkat(build, source, at_fd::cwd(), pathname, at_flags(AT_REMOVEDIR));
  }
  void _rename(Build& build, const IRSource& source, fs::path n1, fs::path n2) noexcept {
    _renameat(build, source, at_fd::cwd(), n1, at_fd::cwd(), n2);
  }
  void _renameat(Build& build,
                 const IRSource& source,
                 at_fd d1,
                 fs::path n1,
                 at_fd d2,
                 fs::path n2) noexcept {
    _renameat2(build, source, d1, n1, d2, n2, rename_flags());
  }
  void _renameat2(Build& build,
                  const IRSource& source,
                  at_fd old_dfd,
                  fs::path oldpath,
                  at_fd new_dfd,
                  fs::path newpath,
                  rename_flags flags) noexcept;
  void _getdents(Build& build, const IRSource& source, int fd) noexcept;
  void _getdents64(Build& build, const IRSource& source, int fd) noexcept {
    _getdents(build, source, fd);
  }

  // Link and Symlink Operations
  void _link(Build& build, const IRSource& source, fs::path oldname, fs::path newname) {
    _linkat(build, source, at_fd::cwd(), oldname, at_fd::cwd(), newname, at_flags());
  }
  void _linkat(Build& build,
               const IRSource& source,
               at_fd old_dfd,
               fs::path oldpath,
               at_fd new_dfd,
               fs::path newpath,
               at_flags flags) noexcept;
  void _symlink(Build& build, const IRSource& source, fs::path oldname, fs::path newname) noexcept {
    _symlinkat(build, source, oldname, at_fd::cwd(), newname);
  }
  void _symlinkat(Build& build,
                  const IRSource& source,
                  fs::path oldname,
                  at_fd newdfd,
                  fs::path newname) noexcept;
  void _readlink(Build& build, const IRSource& source, fs::path path) noexcept {
    _readlinkat(build, source, at_fd::cwd(), path);
  }
  void _readlinkat(Build& build, const IRSource& source, at_fd dfd, fs::path pathname) noexcept;
  void _unlink(Build& build, const IRSource& source, fs::path pathname) noexcept {
    _unlinkat(build, source, at_fd::cwd(), pathname, 0);
  }
  void _unlinkat(Build& build,
                 const IRSource& source,
                 at_fd dfd,
                 fs::path pathname,
                 at_flags flags) noexcept;

  // Socket Operations

  void _accept(Build& build,
               const IRSource& source,
               int sockfd,
               struct sockaddr* addr,
               socklen_t* addrlen) noexcept {
    WARN << "accepting socket " << sockfd;
    _accept4(build, source, sockfd, addr, addrlen, 0);
  }

  void _accept4(Build& build,
                const IRSource& source,
                int sockfd,
                struct sockaddr* addr,
                socklen_t* addrlen,
                int flags) noexcept;

  void _recvfrom(Build& build,
                 const IRSource& source,
                 int sockfd,
                 void* buf,
                 size_t len,
                 int flags,
                 struct sockaddr* src_addr,
                 socklen_t* addrlen) noexcept {
    WARN << "recvfrom(2) not yet implemented. Emulating as a read. Socket: " << sockfd;
    _read(build, source, sockfd);
  }
  void _recvmsg(Build& build,
                const IRSource& source,
                int sockfd,
                struct msghdr msg,
                int flags) noexcept;

  void _sendmsg(Build& build,
                const IRSource& source,
                int sockfd,
                const struct msghdr* msg,
                int flags) noexcept;
  void _sendto(Build& build,
               const IRSource& source,
               int sockfd,
               const void* buf,
               size_t len,
               int flags,
               const struct sockaddr* dest_addr,
               socklen_t addrlen) {
    WARN << "sendto(2) not yet implemented. Emulating as a write. Socket: " << sockfd;
    _write(build, source, sockfd);
  }
  void _socket(Build& build, const IRSource& source, int domain, int type, int protocol) noexcept;
  void _socketpair(Build& build,
                   const IRSource& source,
                   int domain,
                   int type,
                   int protocol,
                   int sv[2]) noexcept;

  // Process State Operations
  void _umask(Build& build, const IRSource& source, mode_t mask) noexcept;
  void _chdir(Build& build, const IRSource& source, fs::path filename) noexcept;
  void _chroot(Build& build, const IRSource& source, fs::path filename) noexcept;
  void _pivot_root(Build& build,
                   const IRSource& source,
                   fs::path new_root,
                   fs::path put_old) noexcept;
  void _fchdir(Build& build, const IRSource& source, int fd) noexcept;
  void _execve(Build& build,
               const IRSource& source,
               fs::path filename,
               std::vector<std::string> args) noexcept {
    _execveat(build, source, at_fd::cwd(), filename, args);
  }
  void _execveat(Build& build,
                 const IRSource& source,
                 at_fd dfd,
                 fs::path filename,
                 std::vector<std::string> args) noexcept;
  void _wait4(Build& build, const IRSource& source, pid_t pid, int* wstatus, int options) noexcept;
  void _waitid(Build& build,
               const IRSource& source,
               idtype_t idtype,
               id_t id,
               siginfo_t* infop,
               int options) noexcept;

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

    operator struct msghdr() { return _thread->readData<struct msghdr>(_val); }

    operator struct cmsghdr() { return _thread->readData<struct cmsghdr>(_val); }

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
  void invokeHandler(void (Thread::*handler)(Output&, const IRSource&),
                     Output& out,
                     const IRSource& source,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, source);
  }

  template <class Output, class T1>
  void invokeHandler(void (Thread::*handler)(Output&, const IRSource&, T1),
                     Output& out,
                     const IRSource& source,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, source, wrap(regs.SYSCALL_ARG1));
  }

  template <class Output, class T1, class T2>
  void invokeHandler(void (Thread::*handler)(Output&, const IRSource&, T1, T2),
                     Output& out,
                     const IRSource& source,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, source, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2));
  }

  template <class Output, class T1, class T2, class T3>
  void invokeHandler(void (Thread::*handler)(Output&, const IRSource&, T1, T2, T3),
                     Output& out,
                     const IRSource& source,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, source, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2),
                       wrap(regs.SYSCALL_ARG3));
  }

  template <class Output, class T1, class T2, class T3, class T4>
  void invokeHandler(void (Thread::*handler)(Output&, const IRSource&, T1, T2, T3, T4),
                     Output& out,
                     const IRSource& source,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, source, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2),
                       wrap(regs.SYSCALL_ARG3), wrap(regs.SYSCALL_ARG4));
  }

  template <class Output, class T1, class T2, class T3, class T4, class T5>
  void invokeHandler(void (Thread::*handler)(Output&, const IRSource&, T1, T2, T3, T4, T5),
                     Output& out,
                     const IRSource& source,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, source, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2),
                       wrap(regs.SYSCALL_ARG3), wrap(regs.SYSCALL_ARG4), wrap(regs.SYSCALL_ARG5));
  }

  template <class Output, class T1, class T2, class T3, class T4, class T5, class T6>
  void invokeHandler(void (Thread::*handler)(Output&, const IRSource&, T1, T2, T3, T4, T5, T6),
                     Output& out,
                     const IRSource& source,
                     const user_regs_struct& regs) {
    (this->*(handler))(out, source, wrap(regs.SYSCALL_ARG1), wrap(regs.SYSCALL_ARG2),
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
  std::stack<std::function<void(Build&, const IRSource&, long)>> _post_syscall_handlers;

  /// Which channel is this thread using for the current trace event? Set to -1 if not using one.
  ssize_t _channel = -1;
};

template <>
struct fmt::formatter<Thread> : fmt::ostream_formatter {};
