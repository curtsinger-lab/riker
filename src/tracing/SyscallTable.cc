#include "SyscallTable.hh"

#include <array>
#include <map>
#include <memory>
#include <string>

#include <syscall.h>

#include "tracing/Flags.hh"
#include "tracing/Thread.hh"
#include "util/log.hh"

using std::array;
using std::map;
using std::shared_ptr;
using std::string;

/**
 * A simple wrapper class around system call arguments that can convert them to known types by
 * reading from a stopped thread.
 */
class SyscallArgWrapper {
 public:
  SyscallArgWrapper(Thread& t, unsigned long val) : _thread(t), _val(val) {}

  // Use static cast for most types
  template <typename T>
  operator T() {
    return static_cast<T>(_val);
  }

  // Get an at_fd from the register value
  operator at_fd() { return at_fd(_val); }

  // Get o_flags from the register value
  operator o_flags() { return o_flags(_val); }

  // Read a string from the thread's memory
  operator string() { return _thread.readString(_val); }

  // Read a vector of strings from the thread's memory
  operator vector<string>() { return _thread.readArgvArray(_val); }

  // Cast directly to pointer types
  template <typename T>
  operator T*() {
    return (T*)_val;
  }

 private:
  Thread& _thread;
  unsigned long _val;
};

// Invoke a no-argument syscall handler
void invoke_handler(void (Thread::*handler)(), Thread& thread, user_regs_struct& regs) {
  (thread.*(handler))();
}

// Invoke a single-argument syscall handler
template <class T1>
void invoke_handler(void (Thread::*handler)(T1 a1), Thread& thread, user_regs_struct& regs) {
  (thread.*(handler))(SyscallArgWrapper(thread, regs.SYSCALL_ARG1));
}

// Invoke a two-argument syscall handler
template <class T1, class T2>
void invoke_handler(void (Thread::*handler)(T1 a1, T2 a2), Thread& thread, user_regs_struct& regs) {
  (thread.*(handler))(SyscallArgWrapper(thread, regs.SYSCALL_ARG1),
                      SyscallArgWrapper(thread, regs.SYSCALL_ARG2));
}

// Invoke a three-argument syscall handler
template <class T1, class T2, class T3>
void invoke_handler(void (Thread::*handler)(T1 a1, T2 a2, T3 a3),
                    Thread& thread,
                    user_regs_struct& regs) {
  (thread.*(handler))(SyscallArgWrapper(thread, regs.SYSCALL_ARG1),
                      SyscallArgWrapper(thread, regs.SYSCALL_ARG2),
                      SyscallArgWrapper(thread, regs.SYSCALL_ARG3));
}

// Invoke a four-argument syscall handler
template <class T1, class T2, class T3, class T4>
void invoke_handler(void (Thread::*handler)(T1 a1, T2 a2, T3 a3, T4 a4),
                    Thread& thread,
                    user_regs_struct& regs) {
  (thread.*(handler))(
      SyscallArgWrapper(thread, regs.SYSCALL_ARG1), SyscallArgWrapper(thread, regs.SYSCALL_ARG2),
      SyscallArgWrapper(thread, regs.SYSCALL_ARG3), SyscallArgWrapper(thread, regs.SYSCALL_ARG4));
}

// Invoke a five-argument syscall handler
template <class T1, class T2, class T3, class T4, class T5>
void invoke_handler(void (Thread::*handler)(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5),
                    Thread& thread,
                    user_regs_struct& regs) {
  (thread.*(handler))(
      SyscallArgWrapper(thread, regs.SYSCALL_ARG1), SyscallArgWrapper(thread, regs.SYSCALL_ARG2),
      SyscallArgWrapper(thread, regs.SYSCALL_ARG3), SyscallArgWrapper(thread, regs.SYSCALL_ARG4),
      SyscallArgWrapper(thread, regs.SYSCALL_ARG5));
}

// Invoke a six-argument syscall handler
template <class T1, class T2, class T3, class T4, class T5, class T6>
void invoke_handler(void (Thread::*handler)(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6),
                    Thread& thread,
                    user_regs_struct& regs) {
  (thread.*(handler))(
      SyscallArgWrapper(thread, regs.SYSCALL_ARG1), SyscallArgWrapper(thread, regs.SYSCALL_ARG2),
      SyscallArgWrapper(thread, regs.SYSCALL_ARG3), SyscallArgWrapper(thread, regs.SYSCALL_ARG4),
      SyscallArgWrapper(thread, regs.SYSCALL_ARG5), SyscallArgWrapper(thread, regs.SYSCALL_ARG6));
}

/// A helper macro for use in the SyscallTable constructor
#define TRACE(name)                                                                          \
  _syscalls[__NR_##name] = SyscallEntry(#name, [](Thread& __thr, user_regs_struct& __regs) { \
    invoke_handler(&Thread::_##name, __thr, __regs);                                         \
  });

/// The maximum number of system calls
#define SYSCALL_COUNT 512

constexpr SyscallTable::SyscallTable() {
  /* 000 */ TRACE(read);
  /* 001 */ TRACE(write);
  /* 002 */ TRACE(open);
  /* 003 */ TRACE(close);
  /* 004 */ TRACE(stat);
  /* 005 */ TRACE(fstat);
  /* 006 */ TRACE(lstat);
  /* 007 */  // skip poll
  /* 008 */  // skip lseek
  /* 009 */ TRACE(mmap);
  /* 010 */  // skip mprotect
  /* 011 */  // skip munmap
  /* 012 */  // skip brk
  /* 013 */  // skip rt_sigaction
  /* 014 */  // skip rt_sigprocmask
  /* 015 */  // skip rt_sigreturn
  /* 016 */  // skip ioctl
  /* 017 */ TRACE(pread64);
  /* 018 */ TRACE(pwrite64);
  /* 019 */ TRACE(readv);
  /* 020 */ TRACE(writev);
  /* 021 */ TRACE(access);
  /* 022 */ TRACE(pipe);
  /* 023 */  // skip select
  /* 024 */  // skip sched_yield
  /* 025 */  // skip mremap
  /* 026 */  // skip msync
  /* 027 */  // skip mincore
  /* 028 */  // skip madvise
  /* 029 */  // skip shmget
  /* 030 */  // skip shmat
  /* 031 */  // skip shmctl
  /* 032 */ TRACE(dup);
  /* 033 */ TRACE(dup2);
  /* 034 */  // skip pause
  /* 035 */  // skip nanosleep
  /* 036 */  // skip getitimer
  /* 037 */  // skip alarm
  /* 038 */  // skip setitimer
  /* 039 */  // skip getpid
  /* 040 */ TRACE(sendfile);
  /* 041 */ TRACE(socket);
  /* 042 */  // TRACE(connect);
  /* 043 */  // TRACE(accept);
  /* 044 */ TRACE(sendto);
  /* 045 */ TRACE(recvfrom);
  /* 046 */ TRACE(sendmsg);
  /* 047 */ TRACE(recvmsg);
  /* 048 */  // TRACE(shutdown);
  /* 049 */  // TRACE(bind);
  /* 050 */  // TRACE(listen);
  /* 051 */  // TRACE(getsockname);
  /* 052 */  // TRACE(getpeername);
  /* 053 */ TRACE(socketpair);
  /* 054 */  // TRACE(setsockopt);
  /* 055 */  // TRACE(getsockopt);
  /* 056 */  // TRACE(clone);
  /* 057 */  // TRACE(fork);
  /* 058 */  // TRACE(vfork);
  /* 059 */ TRACE(execve);
  /* 060 */ TRACE(exit);
  /* 061 */ TRACE(wait4);
  /* 062 */  // skip kill
  /* 063 */  // skip uname
  /* 064 */  // skip semget
  /* 065 */  // skip semop
  /* 066 */  // skip semctl
  /* 067 */  // skip shmdt
  /* 068 */  // skip msgget
  /* 069 */  // skip msgsnd
  /* 070 */  // skip msgrcv
  /* 071 */  // skip msgctl
  /* 072 */ TRACE(fcntl);
  /* 073 */  // skip flock
  /* 074 */  // skip fsync
  /* 075 */  // skip fdatasync
  /* 076 */ TRACE(truncate);
  /* 077 */ TRACE(ftruncate);
  /* 078 */ TRACE(getdents);
  /* 079 */  // skip getcwd
  /* 080 */ TRACE(chdir);
  /* 081 */ TRACE(fchdir);
  /* 082 */ TRACE(rename);
  /* 083 */ TRACE(mkdir);
  /* 084 */ TRACE(rmdir);
  /* 085 */ TRACE(creat);
  /* 086 */ TRACE(link);
  /* 087 */ TRACE(unlink);
  /* 088 */ TRACE(symlink);
  /* 089 */ TRACE(readlink);
  /* 090 */ TRACE(chmod);
  /* 091 */ TRACE(fchmod);
  /* 092 */ TRACE(chown);
  /* 093 */ TRACE(fchown);
  /* 094 */ TRACE(lchown);
  /* 095 */  // skip umask
  /* 096 */  // skip gettimeofday
  /* 097 */  // skip getrlimit
  /* 098 */  // skip getrusage
  /* 099 */  // skip sysinfo
  /* 100 */  // skip times
  /* 101 */  // skip ptrace
  /* 102 */  // skip getuid
  /* 103 */  // skip syslog
  /* 104 */  // skip getgid
  /* 105 */  // skip setuid
  /* 106 */  // skip setgid
  /* 107 */  // skip geteuid
  /* 108 */  // skip getegid
  /* 109 */  // skip setpgid
  /* 110 */  // skip getppid
  /* 111 */  // skip getpgrp
  /* 112 */  // skip setsid
  /* 113 */  // skip setreuid
  /* 114 */  // skip setregid
  /* 115 */  // skip getgroups
  /* 116 */  // skip setgroups
  /* 117 */  // skip setresuid
  /* 118 */  // skip getresuid
  /* 119 */  // skip setresgid
  /* 120 */  // skip getresgid
  /* 121 */  // skip getpgid
  /* 122 */  // skip setfsuid
  /* 123 */  // skip setfsgid
  /* 124 */  // skip getsid
  /* 125 */  // skip capget
  /* 126 */  // skip capset
  /* 127 */  // skip rt_sigpending
  /* 128 */  // skip rt_sigtimedwait
  /* 129 */  // skip rt_sigqueueinfo
  /* 130 */  // skip rt_sigsuspend
  /* 131 */  // skip sigaltstack
  /* 132 */  // skip utime
  /* 133 */ TRACE(mknod);
  /* 134 */  // skip uselib
  /* 135 */  // skip personality
  /* 136 */  // skip ustat
  /* 137 */  // skip statfs
  /* 138 */  // skip fstatfs
  /* 139 */  // skip sysfs
  /* 140 */  // skip getpriority
  /* 141 */  // skip setpriority
  /* 142 */  // skip sched_setparam
  /* 143 */  // skip sched_getparam
  /* 144 */  // skip sched_setscheduler
  /* 145 */  // skip sched_getscheduler
  /* 146 */  // skip sched_get_priority_max
  /* 147 */  // skip sched_get_priority_min
  /* 148 */  // skip sched_rr_get_interval
  /* 149 */  // skip mlock
  /* 150 */  // skip munlock
  /* 151 */  // skip mlockall
  /* 152 */  // skip munlockall
  /* 153 */  // skip vhangup
  /* 154 */  // skip modify_ldt
  /* 155 */ TRACE(pivot_root);
  /* 156 */  // skip _sysctl
  /* 157 */  // skip prctl
  /* 158 */  // skip arch_prctl
  /* 159 */  // skip adjtimex
  /* 160 */  // skip setrlimit
  /* 161 */ TRACE(chroot);
  /* 162 */  // skip sync
  /* 163 */  // skip acct
  /* 164 */  // skip settimeofday
  /* 165 */  // skip mount
  /* 166 */  // skip umount2
  /* 167 */  // skip swapon
  /* 168 */  // skip swapoff
  /* 169 */  // skip reboot
  /* 170 */  // skip sethostname
  /* 171 */  // skip setdomainname
  /* 172 */  // skip iopl
  /* 173 */  // skip ioperm
  /* 174 */  // skip create_module
  /* 175 */  // skip init_module
  /* 176 */  // skip delete_module
  /* 177 */  // skip get_kernel_syms
  /* 178 */  // skip query_module
  /* 179 */  // skip quotactl
  /* 180 */  // skip nfsservctl
  /* 181 */  // skip getpmsg
  /* 182 */  // skip putpmsg
  /* 183 */  // skip afs_syscall
  /* 184 */  // skip tuxcall
  /* 185 */  // skip security
  /* 186 */  // skip gettid
  /* 187 */  // skip readahead
  /* 188 */  // skip setxattr
  /* 189 */  // skip lsetxattr
  /* 190 */  // skip fsetxattr
  /* 191 */  // skip getxattr
  /* 192 */  // skip lgetxattr
  /* 193 */  // skip fgetxattr
  /* 194 */  // skip listxattr
  /* 195 */  // skip llistxattr
  /* 196 */  // skip flistxattr
  /* 197 */  // skip removexattr
  /* 198 */  // skip lremovexattr
  /* 199 */  // skip fremovexattr
  /* 200 */  // skip tkill
  /* 201 */  // skip time
  /* 202 */  // skip futex
  /* 203 */  // skip sched_setaffinity
  /* 204 */  // skip sched_getaffinity
  /* 205 */  // skip set_thread_area
  /* 206 */  // skip io_setup
  /* 207 */  // skip io_destroy
  /* 208 */  // skip io_getevents
  /* 209 */  // skip io_submit
  /* 210 */  // skip io_cancel
  /* 211 */  // skip get_thread_area
  /* 212 */  // skip lookup_dcookie
  /* 213 */  // skip epoll_create
  /* 214 */  // skip epoll_ctl_old
  /* 215 */  // skip epoll_wait_old
  /* 216 */  // skip remap_file_pages
  /* 217 */ TRACE(getdents64);
  /* 218 */  // skip set_tid_address
  /* 219 */  // skip restart_syscall
  /* 220 */  // skip semtimedop
  /* 221 */  // skip fadvise64
  /* 222 */  // skip timer_create
  /* 223 */  // skip timer_settime
  /* 224 */  // skip timer_gettime
  /* 225 */  // skip timer_getoverrun
  /* 226 */  // skip timer_delete
  /* 227 */  // skip clock_settime
  /* 228 */  // skip clock_gettime
  /* 229 */  // skip clock_getres
  /* 230 */  // skip clock_nanosleep
  /* 231 */ TRACE(exit_group);
  /* 232 */  // skip epoll_wait
  /* 233 */  // skip epoll_ctl
  /* 234 */  // skip tgkill
  /* 235 */  // skip utimes
  /* 236 */  // skip vserver
  /* 237 */  // skip mbind
  /* 238 */  // skip set_mempolicy
  /* 239 */  // skip get_mempolicy
  /* 240 */  // skip mq_open
  /* 241 */  // skip mq_unlink
  /* 242 */  // skip mq_timedsend
  /* 243 */  // skip mq_timedreceive
  /* 244 */  // skip mq_notify
  /* 245 */  // skip mq_getsetattr
  /* 246 */  // skip kexec_load
  /* 247 */ TRACE(waitid);
  /* 248 */  // skip add_key
  /* 249 */  // skip request_key
  /* 250 */  // skip keyctl
  /* 251 */  // skip ioprio_set
  /* 252 */  // skip ioprio_get
  /* 253 */  // skip inotify_init
  /* 254 */  // skip inotify_add_watch
  /* 255 */  // skip inotify_rm_watch
  /* 256 */  // skip migrate_pages
  /* 257 */ TRACE(openat);
  /* 258 */ TRACE(mkdirat);
  /* 259 */ TRACE(mknodat);
  /* 260 */ TRACE(fchownat);
  /* 261 */  // skip futimesat
  /* 262 */ TRACE(newfstatat);
  /* 263 */ TRACE(unlinkat);
  /* 264 */ TRACE(renameat);
  /* 265 */ TRACE(linkat);
  /* 266 */ TRACE(symlinkat);
  /* 267 */ TRACE(readlinkat);
  /* 268 */ TRACE(fchmodat);
  /* 269 */ TRACE(faccessat);
  /* 270 */  // skip pselect6
  /* 271 */  // skip ppoll
  /* 272 */  // skip unshare
  /* 273 */  // skip set_robust_list
  /* 274 */  // skip get_robust_list
  /* 275 */ TRACE(splice);
  /* 276 */ TRACE(tee);
  /* 277 */  // skip sync_file_range
  /* 278 */ TRACE(vmsplice);
  /* 279 */  // skip move_pages
  /* 280 */  // skip utimensat
  /* 281 */  // skip epoll_pwait
  /* 282 */  // skip signalfd
  /* 283 */  // skip timerfd_create
  /* 284 */  // skip eventfd
  /* 285 */  // skip fallocate
  /* 286 */  // skip timerfd_settime
  /* 287 */  // skip timerfd_gettime
  /* 288 */  // skip accept4
  /* 289 */  // skip signalfd4
  /* 290 */  // skip eventfd2
  /* 291 */  // skip epoll_create1
  /* 292 */ TRACE(dup3);
  /* 293 */ TRACE(pipe2);
  /* 294 */  // skip inotify_init1
  /* 295 */ TRACE(preadv);
  /* 296 */ TRACE(pwritev);
  /* 297 */  // skip rt_tgsigqueueinfo
  /* 298 */  // skip perf_event_open
  /* 299 */  // skip recvmmsg
  /* 300 */  // skip fanotify_init
  /* 301 */  // skip fanotify_mark
  /* 302 */  // skip prlimit64
  /* 303 */  // skip name_to_handle_at
  /* 304 */  // skip open_by_handle_at
  /* 305 */  // skip clock_adjtime
  /* 306 */  // skip syncfs
  /* 307 */  // skip sendmmsg
  /* 308 */  // skip setns
  /* 309 */  // skip getcpu
  /* 310 */  // skip process_vm_readv
  /* 311 */  // skip process_vm_writev
  /* 312 */  // skip kcmp
  /* 313 */  // skip finit_module
  /* 314 */  // skip sched_setattr
  /* 315 */  // skip sched_getattr
  /* 316 */ TRACE(renameat2);
  /* 317 */  // skip seccomp
  /* 318 */  // skip getrandom
  /* 319 */  // skip memfd_create
  /* 320 */  // skip kexec_file_load
  /* 321 */  // skip bpf
  /* 322 */ TRACE(execveat);
  /* 323 */  // skip userfaultfd
  /* 324 */  // skip membarrier
  /* 325 */  // skip memlock2
  /* 326 */ TRACE(copy_file_range);
  /* 327 */ TRACE(preadv2);
  /* 328 */ TRACE(pwritev2);
  /* 329 */  // skip pkey_mprotect
  /* 330 */  // skip pkey_alloc
  /* 331 */  // skip pkey_free
  /* 332 */ TRACE(statx);
  /* 333 */  // skip io_pgetevents
  /* 334 */  // skip rseq
}

const SyscallTable SyscallTable::_the_table;
