#pragma once

#include <cstddef>
#include <cstdint>
#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/user.h>

#include "core/FileDescriptor.hh"

class BuildGraph;
class Command;
class File;

using std::list;
using std::map;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

struct InitialFdEntry {
  int parent_fd;
  int child_fd;
};

class Tracer {
  class Process;

 public:
  Tracer(BuildGraph& graph) : _graph(graph) {}

  void run(Command* cmd);

 private:
  /// Called when we catch a system call in the traced process
  void handleSyscall(shared_ptr<Process> p);

  /// Called after a traced process issues a clone system call
  void handleClone(shared_ptr<Process> p, int flags);

  /// Called after a traced process issues a fork system call
  void handleFork(shared_ptr<Process> p);

  /// Called when a traced process exits
  void handleExit(shared_ptr<Process> p);

 private:
  class Process {
   public:
    Process(BuildGraph& graph, pid_t pid, string cwd, Command* command,
            map<int, FileDescriptor> fds = {}) :
        _graph(graph),
        _pid(pid),
        _command(command),
        _cwd(cwd),
        _fds(fds) {}

    /// Resume a traced process that is currently stopped
    void resume();

    /// Resume a traced process so it can execute a system call, then stop it and return
    long finishSyscall();

    /// Get the special event message attached to some ptrace stops (clone, fork, etc.)
    unsigned long getEventMessage();

    /// Get the filename of this process' executable
    string getExecutable();

    /// Get the current register state for this process
    user_regs_struct getRegisters();

    /// Read a string from this process' memory
    string readString(uintptr_t tracee_pointer);

    /// Read an 8-byte data value from this process' memory
    uintptr_t readData(uintptr_t tracee_pointer);

    /// Resolve and normalize a path accessed by this process
    string resolvePath(string path, int at = AT_FDCWD, bool follow_links = true);

    /****** Handling for specific system calls ******/

    // After the pid, arguments *must* appear in order. See https://filippo.io/linux-syscall-table/
    // Omitting arguments from the end of the list is fine, but skipping over arguments is not.

    void _read(int fd);
    void _write(int fd);
    void _open(string filename, int flags, mode_t mode) {
      _openat(AT_FDCWD, filename, flags, mode);
    }
    void _close(int fd);
    void _mmap(void* addr, size_t len, int prot, int flags, int fd, off_t off);
    void _pread64(int fd) { _read(fd); }
    void _pwrite64(int fd) { _write(fd); }
    void _readv(int fd) { _read(fd); }
    void _writev(int fd) { _write(fd); }
    void _pipe(int* fds) { _pipe2(fds, 0); }
    int _dup(int fd);
    void _dup2(int oldfd, int newfd) { _dup3(oldfd, newfd, 0); }
    void _sendfile(int out_fd, int in_fd);
    void _exec(string filename, const list<string>& args);
    void _fcntl(int fd, int cmd, unsigned long arg);
    void _truncate(string path, long length);
    void _ftruncate(int fd, long length);
    void _getdents(int fd) { _read(fd); }
    void _chdir(string filename);
    void _fchdir(int fd);
    void _rename(string oldname, string newname) {
      _renameat(AT_FDCWD, oldname, AT_FDCWD, newname);
    }
    void _mkdir(string pathname, mode_t mode) { _mkdirat(AT_FDCWD, pathname, mode); }
    void _rmdir(string pathname) { _unlink(pathname); }
    void _creat(string pathname, mode_t mode) {
      _open(pathname, O_CREAT | O_WRONLY | O_TRUNC, mode);
    }
    void _unlink(string pathname) { _unlinkat(AT_FDCWD, pathname, 0); }
    void _symlink(string oldname, string newname) { _symlinkat(oldname, AT_FDCWD, newname); }
    void _readlink(string path) { _readlinkat(AT_FDCWD, path); }
    void _chmod(string filename, mode_t mode) { _fchmodat(AT_FDCWD, filename, mode, 0); }
    void _fchmod(int fd, mode_t mode) { _write(fd); }
    void _chown(string filename, uid_t user, gid_t group) {
      _fchownat(AT_FDCWD, filename, user, group, 0);
    }
    void _fchown(int fd, uid_t user, gid_t group) { _write(fd); }
    void _lchown(string filename, uid_t user, gid_t group);
    void _mknod(string filename, mode_t mode, unsigned dev) {
      _mknodat(AT_FDCWD, filename, mode, dev);
    }
    void _chroot(string filename);
    void _setxattr(string pathname);
    void _lsetxattr(string pathname);
    void _fsetxattr(int fd) { _write(fd); }
    void _getxattr(string pathname);
    void _lgetxattr(string pathname);
    void _fgetxattr(int fd) { _read(fd); }
    void _listxattr(string pathname) { _getxattr(pathname); }
    void _llistxattr(string pathname) { _lgetxattr(pathname); }
    void _flistxattr(int fd) { _read(fd); }
    void _removexattr(string pathname) { _setxattr(pathname); }
    void _lremovexattr(string pathname) { _lsetxattr(pathname); }
    void _fremovexattr(int fd) { _write(fd); }
    void _getdents64(int fd) { _read(fd); }
    void _openat(int dfd, string filename, int flags, mode_t mode);
    void _mkdirat(int dfd, string pathname, mode_t mode);
    void _mknodat(int dfd, string filename, mode_t mode, unsigned dev);
    void _fchownat(int dfd, string filename, uid_t user, gid_t group, int flag);
    void _unlinkat(int dfd, string pathname, int flag);
    void _renameat(int dfd, string oldname, int newdfd, string newname) {
      _renameat2(dfd, oldname, newdfd, newname, 0);
    }
    void _symlinkat(string oldname, int newdfd, string newname);
    void _readlinkat(int dfd, string pathname);
    void _fchmodat(int dfd, string filename, mode_t mode, int flags);
    void _splice(int fd_in, loff_t off_in, int fd_out, loff_t off_out) { _tee(fd_in, fd_out); }
    void _tee(int fd_in, int fd_out);
    void _vmsplice(int fd) { _write(fd); }
    void _dup3(int oldfd, int newfd, int flags);
    void _pipe2(int* fds, int flags);
    void _preadv(int fd) { _read(fd); }
    void _pwritev(int fd) { _write(fd); }
    void _renameat2(int old_dfd, string oldpath, int new_dfd, string newpath, int flags);
    void _copy_file_range(int fd_in, int _, int fd_out) { _tee(fd_in, fd_out); }
    void _preadv2(int fd) { _read(fd); }
    void _pwritev2(int fd) { _write(fd); }

    BuildGraph& _graph;
    pid_t _pid;
    Command* _command;
    string _cwd;
    string _root;
    set<File*> _mmaps;
    map<int, FileDescriptor> _fds;
  };

 private:
  BuildGraph& _graph;
  map<pid_t, shared_ptr<Process>> _processes;
};

// Skipped syscalls: stat, fstat, lstat, poll, lseek

// Skipped syscalls: mprotect, munmap, brk rt_sigaction, rt_sigprocmask, rt_sigreturn, ioctl

// Skipped syscalls: access (TODO!)

// Skipped syscalls: select, sched_yield, mremap, msync, mincore, madvise, shmget, shmat, shmctl

// Skipped syscalls: pause, nanosleep, getitimer, alarm, setitimer, getpid

// Skipped syscalls: socket, connect, accept, sendto, recvfrom, sendmsg, recvmsg, shutdown, bind
//                   listen, getsockname, getpeername, socketpair, setsockopt, getsockopt

// Skipped syscalls: wait4, kill, uname, semget, semop, semctl, shmdt, msgget, msgsnd, msgrcv,
//                   msgctl

// Skipped syscalls: flock, fsync, fdatasync

// Skipped syscalls: getcwd

// Skipped syscalls: link (TODO!)

// Skipped syscalls: umask, gettimeofday, getrlimit, getrusage, sysinfo, times, ptrace, getuid,
//                   syslog, getgid, setuid, setgid, geteuid, getegid, setpgid, getppid, getpgrp
//                   setsid, setreuid, setregid, getgroups, setroups, setresuid, getresuid
//                   setresgid, getresgid, getpgid, setfsuid, setfsgid, getsid, capget, capset
//                   rt_sigpending, rt_sigtimedwait, rt_sigqueueinfo, rt_sigsuspend, sigaltstack
//                   utime

// Skipped syscalls: uselib, personality, ustat, statfs, fstatfs, sysfs, getpriority, setpriority,
//                   sched_setparam, sched_getparam, sched_setscheduler, sched_getscheduler,
//                   sched_get_priority_max, sched_get_priority_min, sched_rr_get_interval, mlock
//                   munlock, mlockall, munlockall, vhangup, modify_ldt

// Skipped syscalls: pivot_root, _sysctl, prctl, arch_prctl, adjtimex, setrlimit

// Skipped syscalls: sync, acct, settimeofday, mount, umount2, swapon, swapoff, reboot,
//                   sethostname, setdomainname, iopl, ioperm, create_module, init_module,
//                   delete_module, get_kernel_syms, query_module, quotactl, nfsservctl, getpmsg,
//                   putpmsg, afs_syscall, tuxcall, security, gettid, readahead

// Skipped syscalls: tkill, time, futex, sched_setaffinity, sched_getaffinity, set_thread_area,
//                   io_setup, io_destroy, io_getevents, io_submit, io_cancel, get_thread_area,
//                   lookup_dcookie, epoll_create, epoll_ctl_old, epoll_wait_old, remap_file_pages

// Skipped syscalls: set_tid_address, restart_syscall, semtimedop, fadvise64, timer_create,
//                   timer_settime, timer_gettime, timer_getoverrun, timer_delete, clock_settime,
//                   clock_gettime, clock_getres, clock_nanosleep, exit_group, epoll_wait,
//                   epoll_ctl, tgkill, utimes, vserver, mbind, set_mempolicy, get_mempolicy,
//                   mq_open, mq_unlink, mq_timedsend, mq_timedreceive, mq_notify, mq_getsetattr,
//                   kexec_load, waitid, add_key, request_key, keyctl, ioprio_set, ioprio_get,
//                   inotify_init, inotify_add_watch, inotify_rm_watch, migrate_pages

// Skipped syscalls: futimesat, newfstatat

// Skipped syscalls: linkat (TODO!)

// Skipped syscalls: faccessat (TODO!), pselect6, ppoll, unshare (TODO!), set_robust_list,
//                   get_robust_list

// Skipped syscalls: sync_file_range

// Skipped syscalls: move_pages, utimensat, epoll_pwait, signalfd, timerfd_create, eventfd,
//                   fallocate, timerfd_settime, timerfd_gettime, accept4, signalfd4, eventfd2,
//                   epoll_create1

// Skipped syscalls: inotify_init1

// Skipped syscalls: rt_tgsigqueueinfo, perf_event_open, recvmmsg, fanotify_init, fanotify_mark,
// prlimit64, name_to_handle_at (TODO!), open_by_handle_at (TODO!), clock_adjtime, syncfs,
// sendmmsg, setns, getcpu, process_vm_readv, kcmp, finit_module, ... unknown
