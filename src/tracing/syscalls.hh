#pragma once

#include <map>
#include <string>

#include <syscall.h>

using std::map;
using std::string;

#define SYSCALL_NUMBER orig_rax
#define SYSCALL_RETURN rax
#define SYSCALL_ARG1 rdi
#define SYSCALL_ARG2 rsi
#define SYSCALL_ARG3 rdx
#define SYSCALL_ARG4 r10
#define SYSCALL_ARG5 r8
#define SYSCALL_ARG6 r9

#define SYSCALL_ENTRY(name) \
  { __NR_##name, #name }

// Syscall list derived from syscall defines in /usr/include/x86_64-linux-gnu/asm/unistd_64.h

inline static map<uint32_t, string> syscalls = {
    /* 000 */ SYSCALL_ENTRY(read),
    /* 001 */ SYSCALL_ENTRY(write),
    /* 002 */ SYSCALL_ENTRY(open),
    /* 003 */ SYSCALL_ENTRY(close),
    /* 004 */ SYSCALL_ENTRY(stat),
    /* 005 */ SYSCALL_ENTRY(fstat),
    /* 006 */ SYSCALL_ENTRY(lstat),
    /* 007 */  // skip poll
    /* 008 */  // skip lseek
    /* 009 */ SYSCALL_ENTRY(mmap),
    /* 010 */  // skip mprotect
    /* 011 */  // skip munmap
    /* 012 */  // skip brk
    /* 013 */  // skip rt_sigaction
    /* 014 */  // skip rt_sigprocmask
    /* 015 */  // skip rt_sigreturn
    /* 016 */  // skip ioctl
    /* 017 */ SYSCALL_ENTRY(pread64),
    /* 018 */ SYSCALL_ENTRY(pwrite64),
    /* 019 */ SYSCALL_ENTRY(readv),
    /* 020 */ SYSCALL_ENTRY(writev),
    /* 021 */ SYSCALL_ENTRY(access),
    /* 022 */ SYSCALL_ENTRY(pipe),
    /* 023 */  // skip select
    /* 024 */  // skip sched_yield
    /* 025 */  // skip mremap
    /* 026 */  // skip msync
    /* 027 */  // skip mincore
    /* 028 */  // skip madvise
    /* 029 */  // skip shmget
    /* 030 */  // skip shmat
    /* 031 */  // skip shmctl
    /* 032 */ SYSCALL_ENTRY(dup),
    /* 033 */ SYSCALL_ENTRY(dup2),
    /* 034 */  // skip pause
    /* 035 */  // skip nanosleep
    /* 036 */  // skip getitimer
    /* 037 */  // skip alarm
    /* 038 */  // skip setitimer
    /* 039 */  // skip getpid
    /* 040 */ SYSCALL_ENTRY(sendfile),
    /* 041 */  // skip socket
    /* 042 */  // skip connect
    /* 043 */  // skip accept
    /* 044 */  // skip sendto
    /* 045 */  // skip recvfrom
    /* 046 */  // skip sendmsg
    /* 047 */  // skip recvmsg
    /* 048 */  // skip shutdown
    /* 049 */  // skip bind
    /* 050 */  // skip listen
    /* 051 */  // skip getsockname
    /* 052 */  // skip getpeername
    /* 053 */  // skip socketpair
    /* 054 */  // skip setsockopt
    /* 055 */  // skip getsockopt
    /* 056 */  // skip clone
    /* 057 */  // skip fork
    /* 058 */  // skip vfork
    /* 059 */ SYSCALL_ENTRY(execve),
    /* 060 */  // skip exit
    /* 061 */  // skip wait4
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
    /* 072 */ SYSCALL_ENTRY(fcntl),
    /* 073 */  // skip flock
    /* 074 */  // skip fsync
    /* 075 */  // skip fdatasync
    /* 076 */ SYSCALL_ENTRY(truncate),
    /* 077 */ SYSCALL_ENTRY(ftruncate),
    /* 078 */ SYSCALL_ENTRY(getdents),
    /* 079 */  // skip getcwd
    /* 080 */ SYSCALL_ENTRY(chdir),
    /* 081 */ SYSCALL_ENTRY(fchdir),
    /* 082 */ SYSCALL_ENTRY(rename),
    /* 083 */ SYSCALL_ENTRY(mkdir),
    /* 084 */ SYSCALL_ENTRY(rmdir),
    /* 085 */ SYSCALL_ENTRY(creat),
    /* 086 */ SYSCALL_ENTRY(link),
    /* 087 */ SYSCALL_ENTRY(unlink),
    /* 088 */ SYSCALL_ENTRY(symlink),
    /* 089 */ SYSCALL_ENTRY(readlink),
    /* 090 */ SYSCALL_ENTRY(chmod),
    /* 091 */ SYSCALL_ENTRY(fchmod),
    /* 092 */ SYSCALL_ENTRY(chown),
    /* 093 */ SYSCALL_ENTRY(fchown),
    /* 094 */ SYSCALL_ENTRY(lchown),
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
    /* 133 */ SYSCALL_ENTRY(mknod),
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
    /* 155 */ SYSCALL_ENTRY(pivot_root),
    /* 156 */  // skip _sysctl
    /* 157 */  // skip prctl
    /* 158 */  // skip arch_prctl
    /* 159 */  // skip adjtimex
    /* 160 */  // skip setrlimit
    /* 161 */ SYSCALL_ENTRY(chroot),
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
    /* 217 */ SYSCALL_ENTRY(getdents64),
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
    /* 231 */  // skip exit_group
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
    /* 247 */  // skip waitid
    /* 248 */  // skip add_key
    /* 249 */  // skip request_key
    /* 250 */  // skip keyctl
    /* 251 */  // skip ioprio_set
    /* 252 */  // skip ioprio_get
    /* 253 */  // skip inotify_init
    /* 254 */  // skip inotify_add_watch
    /* 255 */  // skip inotify_rm_watch
    /* 256 */  // skip migrate_pages
    /* 257 */ SYSCALL_ENTRY(openat),
    /* 258 */ SYSCALL_ENTRY(mkdirat),
    /* 259 */ SYSCALL_ENTRY(mknodat),
    /* 260 */ SYSCALL_ENTRY(fchownat),
    /* 261 */  // skip futimesat
    /* 262 */ SYSCALL_ENTRY(newfstatat),
    /* 263 */ SYSCALL_ENTRY(unlinkat),
    /* 264 */ SYSCALL_ENTRY(renameat),
    /* 265 */ SYSCALL_ENTRY(linkat),
    /* 266 */ SYSCALL_ENTRY(symlinkat),
    /* 267 */ SYSCALL_ENTRY(readlinkat),
    /* 268 */ SYSCALL_ENTRY(fchmodat),
    /* 269 */ SYSCALL_ENTRY(faccessat),
    /* 270 */  // skip pselect6
    /* 271 */  // skip ppoll
    /* 272 */  // skip unshare
    /* 273 */  // skip set_robust_list
    /* 274 */  // skip get_robust_list
    /* 275 */ SYSCALL_ENTRY(splice),
    /* 276 */ SYSCALL_ENTRY(tee),
    /* 277 */  // skip sync_file_range
    /* 278 */ SYSCALL_ENTRY(vmsplice),
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
    /* 292 */ SYSCALL_ENTRY(dup3),
    /* 293 */ SYSCALL_ENTRY(pipe2),
    /* 294 */  // skip inotify_init1
    /* 295 */ SYSCALL_ENTRY(preadv),
    /* 296 */ SYSCALL_ENTRY(pwritev),
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
    /* 316 */ SYSCALL_ENTRY(renameat2),
    /* 317 */  // skip seccomp
    /* 318 */  // skip getrandom
    /* 319 */  // skip memfd_create
    /* 320 */  // skip kexec_file_load
    /* 321 */  // skip bpf
    /* 322 */ SYSCALL_ENTRY(execveat),
    /* 323 */  // skip userfaultfd
    /* 324 */  // skip membarrier
    /* 325 */  // skip memlock2
    /* 326 */ SYSCALL_ENTRY(copy_file_range),
    /* 327 */ SYSCALL_ENTRY(preadv2),
    /* 328 */ SYSCALL_ENTRY(pwritev2),
    /* 329 */  // skip pkey_mprotect
    /* 330 */  // skip pkey_alloc
    /* 331 */  // skip pkey_free
    /* 332 */ SYSCALL_ENTRY(statx),
    /* 333 */  // skip io_pgetevents
    /* 334 */  // skip rseq
};
