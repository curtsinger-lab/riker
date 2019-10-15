#pragma once

#include <list>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/types.h>

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
 public:
  Tracer(BuildGraph& graph) : _graph(graph) {}

  void run(Command* cmd);

 private:
  /****** Handling for ptrace events ******/
  void resume(pid_t pid);

  long finishSyscall(pid_t pid);
  
  unsigned long getEventMessage(pid_t pid);

  void handleSyscall(pid_t pid);

  /****** Handling for specific system calls ******/

  // After the pid, arguments *must* appear in order. See https://filippo.io/linux-syscall-table/
  // Omitting arguments from the end of the list is fine, but skipping over arguments is not.

  /* 000 */ void _read(pid_t pid, int fd);
  /* 001 */ void _write(pid_t pid, int fd);
  /* 002 */ void _open(pid_t pid, string filename, int flags, mode_t mode) {
    _openat(pid, AT_FDCWD, filename, flags, mode);
  }
  /* 003 */ void _close(pid_t pid, int fd);

  // Skipped syscalls: stat, fstat, lstat, poll, lseek

  /* 009 */ void _mmap(pid_t pid, void* addr, size_t len, int prot, int flags, int fd, off_t off);

  // Skipped syscalls: mprotect, munmap, brk rt_sigaction, rt_sigprocmask, rt_sigreturn, ioctl

  /* 017 */ void _pread64(pid_t pid, int fd) { _read(pid, fd); }
  /* 018 */ void _pwrite64(pid_t pid, int fd) { _write(pid, fd); }
  /* 019 */ void _readv(pid_t pid, int fd) { _read(pid, fd); }
  /* 020 */ void _writev(pid_t pid, int fd) { _write(pid, fd); }

  // Skipped syscalls: access (TODO!)

  /* 022 */ void _pipe(pid_t pid, int* fds) { _pipe2(pid, fds, 0); }

  // Skipped syscalls: select, sched_yield, mremap, msync, mincore, madvise, shmget, shmat, shmctl

  /* 032 */ void _dup(pid_t pid, int fd);
  /* 033 */ void _dup2(pid_t pid, int oldfd, int newfd);

  // Skipped syscalls: pause, nanosleep, getitimer, alarm, setitimer, getpid

  /* 040 */ void _sendfile(pid_t pid, int out_fd, int in_fd);  // Skipped args: offset, count

  // Skipped syscalls: socket, connect, accept, sendto, recvfrom, sendmsg, recvmsg, shutdown, bind
  //                   listen, getsockname, getpeername, socketpair, setsockopt, getsockopt

  /* 056 */ void _clone(pid_t pid, int flags);
  /* 057 */ void _fork(pid_t pid);
  /* 058 */ void _vfork(pid_t pid) { _fork(pid); }
  /* 059 */ void _execve(pid_t pid, string filename, const list<string>& args);
  /* 060 */ void _exit(pid_t pid);

  // Skipped syscalls: wait4, kill, uname, semget, semop, semctl, shmdt, msgget, msgsnd, msgrcv,
  //                   msgctl

  /* 072 */ void _fcntl(pid_t pid, int fd, int cmd, unsigned long arg);

  // Skipped syscalls: flock, fsync, fdatasync

  /* 076 */ void _truncate(pid_t pid, string path, long length);
  /* 077 */ void _ftruncate(pid_t pid, int fd, long length);
  /* 078 */ void _getdents(pid_t pid, int fd);

  // Skipped syscalls: getcwd

  /* 080 */ void _chdir(pid_t pid, string filename);
  /* 081 */ void _fchdir(pid_t pid, int fd);
  /* 082 */ void _rename(pid_t pid, string oldname, string newname);
  /* 083 */ void _mkdir(pid_t pid, string pathname, mode_t mode);
  /* 084 */ void _rmdir(pid_t pid, string pathname);
  /* 085 */ void _creat(pid_t pid, string pathname, mode_t mode);

  // Skipped syscalls: link (TODO!)

  /* 087 */ void _unlink(pid_t pid, string pathname);
  /* 088 */ void _symlink(pid_t pid, string oldname, string newname);
  /* 089 */ void _readlink(pid_t pid, string path);
  /* 090 */ void _chmod(pid_t pid, string filename, mode_t mode);
  /* 091 */ void _fchmod(pid_t pid, int fd, mode_t mode);
  /* 092 */ void _chown(pid_t pid, string filename, uid_t user, gid_t group);
  /* 093 */ void _fchown(pid_t pid, int fd, uid_t user, gid_t group);
  /* 094 */ void _lchown(pid_t pid, string filename, uid_t user, gid_t group);

  // Skipped syscalls: umask, gettimeofday, getrlimit, getrusage, sysinfo, times, ptrace, getuid,
  //                   syslog, getgid, setuid, setgid, geteuid, getegid, setpgid, getppid, getpgrp
  //                   setsid, setreuid, setregid, getgroups, setroups, setresuid, getresuid
  //                   setresgid, getresgid, getpgid, setfsuid, setfsgid, getsid, capget, capset
  //                   rt_sigpending, rt_sigtimedwait, rt_sigqueueinfo, rt_sigsuspend, sigaltstack
  //                   utime

  /* 133 */ void _mknod(pid_t pid, string filename, mode_t mode, unsigned dev);

  // Skipped syscalls: uselib, personality, ustat, statfs, fstatfs, sysfs, getpriority, setpriority,
  //                   sched_setparam, sched_getparam, sched_setscheduler, sched_getscheduler,
  //                   sched_get_priority_max, sched_get_priority_min, sched_rr_get_interval, mlock
  //                   munlock, mlockall, munlockall, vhangup, modify_ldt

  // Skipped syscalls: pivot_root, _sysctl, prctl, arch_prctl, adjtimex, setrlimit

  /* 161 */ void _chroot(pid_t pid, string filename);

  // Skipped syscalls: sync, acct, settimeofday, mount, umount2, swapon, swapoff, reboot,
  //                   sethostname, setdomainname, iopl, ioperm, create_module, init_module,
  //                   delete_module, get_kernel_syms, query_module, quotactl, nfsservctl, getpmsg,
  //                   putpmsg, afs_syscall, tuxcall, security, gettid, readahead

  /* 188 */ void _setxattr(pid_t pid, string pathname);
  /* 189 */ void _lsetxattr(pid_t pid, string pathname);
  /* 190 */ void _fsetxattr(pid_t pid, int fd);
  /* 191 */ void _getxattr(pid_t pid, string pathname);
  /* 192 */ void _lgetxattr(pid_t pid, string pathname);
  /* 193 */ void _fgetxattr(pid_t pid, int fd);
  /* 194 */ void _listxattr(pid_t pid, string pathname);
  /* 195 */ void _llistxattr(pid_t pid, string pathname);
  /* 196 */ void _flistxattr(pid_t pid, int fd);
  /* 197 */ void _removexattr(pid_t pid, string pathname);
  /* 198 */ void _lremovexattr(pid_t pid, string pathname);
  /* 199 */ void _fremovexattr(pid_t pid, int fd);

  // Skipped syscalls: tkill, time, futex, sched_setaffinity, sched_getaffinity, set_thread_area,
  //                   io_setup, io_destroy, io_getevents, io_submit, io_cancel, get_thread_area,
  //                   lookup_dcookie, epoll_create, epoll_ctl_old, epoll_wait_old, remap_file_pages

  /* 217 */ void _getdents64(pid_t pid, int fd);

  // Skipped syscalls: set_tid_address, restart_syscall, semtimedop, fadvise64, timer_create,
  //                   timer_settime, timer_gettime, timer_getoverrun, timer_delete, clock_settime,
  //                   clock_gettime, clock_getres, clock_nanosleep, exit_group, epoll_wait,
  //                   epoll_ctl, tgkill, utimes, vserver, mbind, set_mempolicy, get_mempolicy,
  //                   mq_open, mq_unlink, mq_timedsend, mq_timedreceive, mq_notify, mq_getsetattr,
  //                   kexec_load, waitid, add_key, request_key, keyctl, ioprio_set, ioprio_get,
  //                   inotify_init, inotify_add_watch, inotify_rm_watch, migrate_pages

  /* 257 */ void _openat(pid_t pid, int dfd, string filename, int flags, mode_t mode);
  /* 258 */ void _mkdirat(pid_t pid, int dfd, string pathname, mode_t mode);
  /* 259 */ void _mknodat(pid_t pid, int dfd, string filename, mode_t mode, unsigned dev);
  /* 260 */ void _fchownat(pid_t pid, int dfd, string filename, uid_t user, gid_t group, int flag);

  // Skipped syscalls: futimesat, newfstatat

  /* 263 */ void _unlinkat(pid_t pid, int dfd, string pathname, int flag);
  /* 264 */ void _renameat(pid_t pid, int dfd, string oldname, int newdfd, string newname);

  // Skipped syscalls: linkat (TODO!)

  /* 266 */ void _symlinkat(pid_t pid, string oldname, int newdfd, string newname);
  /* 267 */ void _readlinkat(pid_t pid, int dfd, string pathname);
  /* 268 */ void _fchmodat(pid_t pid, int dfd, string filename, mode_t mode);

  // Skipped syscalls: faccessat (TODO!), pselect6, ppoll, unshare (TODO!), set_robust_list,
  //                   get_robust_list

  /* 275 */ void _splice(pid_t pid, int fd_in, loff_t off_in, int fd_out, loff_t off_out);
  /* 276 */ void _tee(pid_t pid, int fdin, int fdout, size_t len);

  // Skipped syscalls: sync_file_range

  /* 278 */ void _vmsplice(pid_t pid, int fd) { _write(pid, fd); }

  // Skipped syscalls: move_pages, utimensat, epoll_pwait, signalfd, timerfd_create, eventfd,
  //                   fallocate, timerfd_settime, timerfd_gettime, accept4, signalfd4, eventfd2,
  //                   epoll_create1

  /* 292 */ void _dup3(pid_t pid, int oldfd, int newfd, int flags);
  /* 293 */ void _pipe2(pid_t pid, int* fds, int flags);

  // Skipped syscalls: inotify_init1

  /* 295 */ void _preadv(pid_t pid, int fd) { _read(pid, fd); }
  /* 296 */ void _pwritev(pid_t pid, int fd) { _write(pid, fd); }

  // Skipped syscalls: rt_tgsigqueueinfo, perf_event_open, recvmmsg, fanotify_init, fanotify_mark,
  // prlimit64, name_to_handle_at (TODO!), open_by_handle_at (TODO!), clock_adjtime, syncfs,
  // sendmmsg, setns, getcpu, process_vm_readv, kcmp, finit_module, ... unknown

  /* 316 */ void _renameat2(pid_t pid, int old_dfd, string oldpath, int new_dfd, string newpath);

  /* 326 */ void _copy_file_range(pid_t pid, int fd_in, int _, int fd_out);
  /* 327 */ void _preadv2(pid_t pid, int fd) { _read(pid, fd); }
  /* 328 */ void _pwritev2(pid_t pid, int fd) { _write(pid, fd); }

 private:
  struct Process {
    Process(pid_t pid, string cwd, Command* command, map<int, FileDescriptor> fds = {}) :
        _pid(pid),
        _command(command),
        _cwd(cwd),
        _fds(fds) {}

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
