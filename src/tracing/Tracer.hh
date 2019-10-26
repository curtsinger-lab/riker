#pragma once

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
  /****** Handling for ptrace events ******/
  void handleSyscall(shared_ptr<Process> p);

  /****** Handling for specific system calls ******/

  // After the pid, arguments *must* appear in order. See https://filippo.io/linux-syscall-table/
  // Omitting arguments from the end of the list is fine, but skipping over arguments is not.

  /* 000 */ void _read(shared_ptr<Process> p, int fd);
  /* 001 */ void _write(shared_ptr<Process> p, int fd);
  /* 002 */ void _open(shared_ptr<Process> p, string filename, int flags, mode_t mode) {
    _openat(p, AT_FDCWD, filename, flags, mode);
  }
  /* 003 */ void _close(shared_ptr<Process> p, int fd);

  // Skipped syscalls: stat, fstat, lstat, poll, lseek

  /* 009 */ void _mmap(shared_ptr<Process> p, void* addr, size_t len, int prot, int flags, int fd,
                       off_t off);

  // Skipped syscalls: mprotect, munmap, brk rt_sigaction, rt_sigprocmask, rt_sigreturn, ioctl

  /* 017 */ void _pread64(shared_ptr<Process> p, int fd) { _read(p, fd); }
  /* 018 */ void _pwrite64(shared_ptr<Process> p, int fd) { _write(p, fd); }
  /* 019 */ void _readv(shared_ptr<Process> p, int fd) { _read(p, fd); }
  /* 020 */ void _writev(shared_ptr<Process> p, int fd) { _write(p, fd); }

  // Skipped syscalls: access (TODO!)

  /* 022 */ void _pipe(shared_ptr<Process> p, int* fds) { _pipe2(p, fds, 0); }

  // Skipped syscalls: select, sched_yield, mremap, msync, mincore, madvise, shmget, shmat, shmctl

  /* 032 */ int _dup(shared_ptr<Process> p, int fd);
  /* 033 */ void _dup2(shared_ptr<Process> p, int oldfd, int newfd);

  // Skipped syscalls: pause, nanosleep, getitimer, alarm, setitimer, getpid

  /* 040 */ void _sendfile(shared_ptr<Process> p, int out_fd,
                           int in_fd);  // Skipped args: offset, count

  // Skipped syscalls: socket, connect, accept, sendto, recvfrom, sendmsg, recvmsg, shutdown, bind
  //                   listen, getsockname, getpeername, socketpair, setsockopt, getsockopt

  /* 056 */ void _clone(shared_ptr<Process> p, int flags);
  /* 057 */ void _fork(shared_ptr<Process> p);
  /* 058 */ void _vfork(shared_ptr<Process> p) { _fork(p); }
  /* 059 */ void _execve(shared_ptr<Process> p, string filename, const list<string>& args);
  /* 060 */ void _exit(shared_ptr<Process> p);

  // Skipped syscalls: wait4, kill, uname, semget, semop, semctl, shmdt, msgget, msgsnd, msgrcv,
  //                   msgctl

  /* 072 */ void _fcntl(shared_ptr<Process> p, int fd, int cmd, unsigned long arg);

  // Skipped syscalls: flock, fsync, fdatasync

  /* 076 */ void _truncate(shared_ptr<Process> p, string path, long length);
  /* 077 */ void _ftruncate(shared_ptr<Process> p, int fd, long length);
  /* 078 */ void _getdents(shared_ptr<Process> p, int fd) { _read(p, fd); }

  // Skipped syscalls: getcwd

  /* 080 */ void _chdir(shared_ptr<Process> p, string filename);
  /* 081 */ void _fchdir(shared_ptr<Process> p, int fd);
  /* 082 */ void _rename(shared_ptr<Process> p, string oldname, string newname) {
    _renameat(p, AT_FDCWD, oldname, AT_FDCWD, newname);
  }
  /* 083 */ void _mkdir(shared_ptr<Process> p, string pathname, mode_t mode) {
    _mkdirat(p, AT_FDCWD, pathname, mode);
  }
  /* 084 */ void _rmdir(shared_ptr<Process> p, string pathname) { _unlink(p, pathname); }
  /* 085 */ void _creat(shared_ptr<Process> p, string pathname, mode_t mode) {
    _open(p, pathname, O_CREAT | O_WRONLY | O_TRUNC, mode);
  }

  // Skipped syscalls: link (TODO!)

  /* 087 */ void _unlink(shared_ptr<Process> p, string pathname) {
    _unlinkat(p, AT_FDCWD, pathname, 0);
  }
  /* 088 */ void _symlink(shared_ptr<Process> p, string oldname, string newname) {
    _symlinkat(p, oldname, AT_FDCWD, newname);
  }
  /* 089 */ void _readlink(shared_ptr<Process> p, string path) { _readlinkat(p, AT_FDCWD, path); }
  /* 090 */ void _chmod(shared_ptr<Process> p, string filename, mode_t mode) {
    _fchmodat(p, AT_FDCWD, filename, mode, 0);
  }
  /* 091 */ void _fchmod(shared_ptr<Process> p, int fd, mode_t mode) { _write(p, fd); }
  /* 092 */ void _chown(shared_ptr<Process> p, string filename, uid_t user, gid_t group) {
    _fchownat(p, AT_FDCWD, filename, user, group, 0);
  }
  /* 093 */ void _fchown(shared_ptr<Process> p, int fd, uid_t user, gid_t group) {
    _write(p, fd);
  }
  /* 094 */ void _lchown(shared_ptr<Process> p, string filename, uid_t user, gid_t group);

  // Skipped syscalls: umask, gettimeofday, getrlimit, getrusage, sysinfo, times, ptrace, getuid,
  //                   syslog, getgid, setuid, setgid, geteuid, getegid, setpgid, getppid, getpgrp
  //                   setsid, setreuid, setregid, getgroups, setroups, setresuid, getresuid
  //                   setresgid, getresgid, getpgid, setfsuid, setfsgid, getsid, capget, capset
  //                   rt_sigpending, rt_sigtimedwait, rt_sigqueueinfo, rt_sigsuspend, sigaltstack
  //                   utime

  /* 133 */ void _mknod(shared_ptr<Process> p, string filename, mode_t mode, unsigned dev) {
    _mknodat(p, AT_FDCWD, filename, mode, dev);
  }

  // Skipped syscalls: uselib, personality, ustat, statfs, fstatfs, sysfs, getpriority, setpriority,
  //                   sched_setparam, sched_getparam, sched_setscheduler, sched_getscheduler,
  //                   sched_get_priority_max, sched_get_priority_min, sched_rr_get_interval, mlock
  //                   munlock, mlockall, munlockall, vhangup, modify_ldt

  // Skipped syscalls: pivot_root, _sysctl, prctl, arch_prctl, adjtimex, setrlimit

  /* 161 */ void _chroot(shared_ptr<Process> p, string filename);

  // Skipped syscalls: sync, acct, settimeofday, mount, umount2, swapon, swapoff, reboot,
  //                   sethostname, setdomainname, iopl, ioperm, create_module, init_module,
  //                   delete_module, get_kernel_syms, query_module, quotactl, nfsservctl, getpmsg,
  //                   putpmsg, afs_syscall, tuxcall, security, gettid, readahead

  /* 188 */ void _setxattr(shared_ptr<Process> p, string pathname);
  /* 189 */ void _lsetxattr(shared_ptr<Process> p, string pathname);
  /* 190 */ void _fsetxattr(shared_ptr<Process> p, int fd) { _write(p, fd); }
  /* 191 */ void _getxattr(shared_ptr<Process> p, string pathname);
  /* 192 */ void _lgetxattr(shared_ptr<Process> p, string pathname);
  /* 193 */ void _fgetxattr(shared_ptr<Process> p, int fd) { _read(p, fd); }
  /* 194 */ void _listxattr(shared_ptr<Process> p, string pathname) { _getxattr(p, pathname); }
  /* 195 */ void _llistxattr(shared_ptr<Process> p, string pathname) { _lgetxattr(p, pathname); }
  /* 196 */ void _flistxattr(shared_ptr<Process> p, int fd) { _read(p, fd); }
  /* 197 */ void _removexattr(shared_ptr<Process> p, string pathname) { _setxattr(p, pathname); }
  /* 198 */ void _lremovexattr(shared_ptr<Process> p, string pathname) {
    _lsetxattr(p, pathname);
  }
  /* 199 */ void _fremovexattr(shared_ptr<Process> p, int fd) { _write(p, fd); }

  // Skipped syscalls: tkill, time, futex, sched_setaffinity, sched_getaffinity, set_thread_area,
  //                   io_setup, io_destroy, io_getevents, io_submit, io_cancel, get_thread_area,
  //                   lookup_dcookie, epoll_create, epoll_ctl_old, epoll_wait_old, remap_file_pages

  /* 217 */ void _getdents64(shared_ptr<Process> p, int fd) { _read(p, fd); }

  // Skipped syscalls: set_tid_address, restart_syscall, semtimedop, fadvise64, timer_create,
  //                   timer_settime, timer_gettime, timer_getoverrun, timer_delete, clock_settime,
  //                   clock_gettime, clock_getres, clock_nanosleep, exit_group, epoll_wait,
  //                   epoll_ctl, tgkill, utimes, vserver, mbind, set_mempolicy, get_mempolicy,
  //                   mq_open, mq_unlink, mq_timedsend, mq_timedreceive, mq_notify, mq_getsetattr,
  //                   kexec_load, waitid, add_key, request_key, keyctl, ioprio_set, ioprio_get,
  //                   inotify_init, inotify_add_watch, inotify_rm_watch, migrate_pages

  /* 257 */ void _openat(shared_ptr<Process> p, int dfd, string filename, int flags, mode_t mode);
  /* 258 */ void _mkdirat(shared_ptr<Process> p, int dfd, string pathname, mode_t mode);
  /* 259 */ void _mknodat(shared_ptr<Process> p, int dfd, string filename, mode_t mode,
                          unsigned dev);
  /* 260 */ void _fchownat(shared_ptr<Process> p, int dfd, string filename, uid_t user, gid_t group,
                           int flag);

  // Skipped syscalls: futimesat, newfstatat

  /* 263 */ void _unlinkat(shared_ptr<Process> p, int dfd, string pathname, int flag);
  /* 264 */ void _renameat(shared_ptr<Process> p, int dfd, string oldname, int newdfd,
                           string newname) {
    _renameat2(p, dfd, oldname, newdfd, newname, 0);
  }

  // Skipped syscalls: linkat (TODO!)

  /* 266 */ void _symlinkat(shared_ptr<Process> p, string oldname, int newdfd, string newname);
  /* 267 */ void _readlinkat(shared_ptr<Process> p, int dfd, string pathname);
  /* 268 */ void _fchmodat(shared_ptr<Process> p, int dfd, string filename, mode_t mode, int flags);

  // Skipped syscalls: faccessat (TODO!), pselect6, ppoll, unshare (TODO!), set_robust_list,
  //                   get_robust_list

  /* 275 */ void _splice(shared_ptr<Process> p, int fd_in, loff_t off_in, int fd_out,
                         loff_t off_out);
  /* 276 */ void _tee(shared_ptr<Process> p, int fdin, int fdout, size_t len);

  // Skipped syscalls: sync_file_range

  /* 278 */ void _vmsplice(shared_ptr<Process> p, int fd) { _write(p, fd); }

  // Skipped syscalls: move_pages, utimensat, epoll_pwait, signalfd, timerfd_create, eventfd,
  //                   fallocate, timerfd_settime, timerfd_gettime, accept4, signalfd4, eventfd2,
  //                   epoll_create1

  /* 292 */ void _dup3(shared_ptr<Process> p, int oldfd, int newfd, int flags);
  /* 293 */ void _pipe2(shared_ptr<Process> p, int* fds, int flags);

  // Skipped syscalls: inotify_init1

  /* 295 */ void _preadv(shared_ptr<Process> p, int fd) { _read(p, fd); }
  /* 296 */ void _pwritev(shared_ptr<Process> p, int fd) { _write(p, fd); }

  // Skipped syscalls: rt_tgsigqueueinfo, perf_event_open, recvmmsg, fanotify_init, fanotify_mark,
  // prlimit64, name_to_handle_at (TODO!), open_by_handle_at (TODO!), clock_adjtime, syncfs,
  // sendmmsg, setns, getcpu, process_vm_readv, kcmp, finit_module, ... unknown

  /* 316 */ void _renameat2(shared_ptr<Process> p, int old_dfd, string oldpath, int new_dfd,
                            string newpath, int flags);

  /* 326 */ void _copy_file_range(shared_ptr<Process> p, int fd_in, int _, int fd_out);
  /* 327 */ void _preadv2(shared_ptr<Process> p, int fd) { _read(p, fd); }
  /* 328 */ void _pwritev2(shared_ptr<Process> p, int fd) { _write(p, fd); }

 private:
  class Process {
   public:
    Process(pid_t pid, string cwd, Command* command, map<int, FileDescriptor> fds = {}) :
        _pid(pid),
        _command(command),
        _cwd(cwd),
        _fds(fds) {}

    void resume();

    long finishSyscall();

    unsigned long getEventMessage();
    
    string resolvePath(string path, int at = AT_FDCWD, bool follow_links = true);
    
    string getExecutable();
    
    user_regs_struct getRegisters();
    
    string readString(uintptr_t tracee_pointer);
    
    uintptr_t readData(uintptr_t tracee_pointer);

    pid_t _pid;
    Command* _command;
    string _cwd;
    string _root;
    set<shared_ptr<File>> _mmaps;
    map<int, FileDescriptor> _fds;
  };

 private:
  BuildGraph& _graph;
  map<pid_t, shared_ptr<Process>> _processes;
};
