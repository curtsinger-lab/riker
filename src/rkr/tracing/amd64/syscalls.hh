#pragma once

#if !defined(TRACE)
#error "syscalls.hh was included without the TRACE macro"
#endif

/* 000 */ // skip io_setup
/* 001 */ // skip io_destroy
/* 002 */ // skip io_submit
/* 003 */ // skip io_cancel
/* 004 */ // skip io_getevents
/* 005 */ // skip setxattr
/* 006 */ // skip lsetxattr
/* 007 */ // skip fsetxattr
/* 008 */ // skip getxattr
/* 009 */ // skip lgetxattr
/* 010 */ // skip fgetxattr
/* 011 */ // skip listxattr
/* 012 */ // skip llistxattr
/* 013 */ // skip flistxattr
/* 014 */ // skip removexattr
/* 015 */ // skip lremovexattr
/* 016 */ // skip fremovexattr
/* 017 */ // skip getcwd
/* 018 */ // skip lookup_dcookie
/* 019 */ // skip eventfd2
/* 020 */ // skip epoll_create1
/* 021 */ // skip epoll_ctl
/* 022 */ // skip epoll_pwait
/* 023 */ TRACE(dup);
/* 024 */ TRACE(dup3);
/* 025 */ TRACE(fcntl);
/* 026 */ // skip inotify_init1
/* 027 */ // skip inotify_add_watch
/* 028 */ // skip inotify_rm_watch
/* 029 */ // skip ioctl
/* 030 */ // skip ioprio_set
/* 031 */ // skip ioprio_get
/* 032 */ // skip flock
/* 033 */ TRACE(mknodat);
/* 034 */ TRACE(mkdirat);
/* 035 */ TRACE(unlinkat);
/* 036 */ TRACE(symlinkat);
/* 037 */ TRACE(linkat);
/* 038 */ TRACE(renameat);
/* 039 */ // skip umount2
/* 040 */ // skip mount
/* 041 */ TRACE(pivot_root);
/* 042 */ // skip nfsservctl
/* 043 */ // skip statfs
/* 044 */ // skip fstatfs
/* 045 */ TRACE(truncate);
/* 046 */ TRACE(ftruncate);
/* 047 */ // skip fallocate
/* 048 */ TRACE(faccessat);
/* 049 */ TRACE(chdir);
/* 050 */ TRACE(fchdir);
/* 051 */ TRACE(chroot);
/* 052 */ TRACE(fchmod);
/* 053 */ TRACE(fchmodat);
/* 054 */ TRACE(fchownat);
/* 055 */ TRACE(fchown);
/* 056 */ TRACE(openat);
/* 057 */ TRACE(close);
/* 058 */ // skip vhangup
/* 059 */ TRACE(pipe2);
/* 060 */ // skip quotactl
/* 061 */ TRACE(getdents64);
/* 062 */ // skip lseek
/* 063 */ TRACE(read);
/* 064 */ TRACE(write);
/* 065 */ TRACE(readv);
/* 066 */ TRACE(writev);
/* 067 */ TRACE(pread64);
/* 068 */ TRACE(pwrite64);
/* 069 */ TRACE(preadv);
/* 070 */ TRACE(pwritev);
/* 071 */ TRACE(sendfile);
/* 072 */ // skip pselect6
/* 073 */ // skip ppoll
/* 074 */ // skip signalfd4
/* 075 */ TRACE(vmsplice);
/* 076 */ TRACE(splice);
/* 077 */ TRACE(tee);
/* 078 */ TRACE(readlinkat);
/* 079 */ TRACE(fstatat);
/* 080 */ TRACE(fstat);
/* 081 */ // skip sync
/* 082 */ // skip fsync
/* 083 */ // skip fdatasync
/* 084 */ // skip sync_file_range2
/* 084 */ // skip sync_file_range
/* 085 */ // skip timerfd_create
/* 086 */ // skip timerfd_settime
/* 087 */ // skip timerfd_gettime
/* 088 */ // skip utimensat
/* 089 */ // skip acct
/* 090 */ // skip capget
/* 091 */ // skip capset
/* 092 */ // skip personality
/* 093 */ // skip exit
/* 094 */ // skip exit_group
/* 095 */ TRACE(waitid);
/* 096 */ // skip set_tid_address
/* 097 */ // skip unshare
/* 098 */ // skip futex
/* 099 */ // skip set_robust_list
/* 100 */ // skip get_robust_list
/* 101 */ // skip nanosleep
/* 102 */ // skip getitimer
/* 103 */ // skip setitimer
/* 104 */ // skip kexec_load
/* 105 */ // skip init_module
/* 106 */ // skip delete_module
/* 107 */ // skip timer_create
/* 108 */ // skip timer_gettime
/* 109 */ // skip timer_getoverrun
/* 110 */ // skip timer_settime
/* 111 */ // skip timer_delete
/* 112 */ // skip clock_settime
/* 113 */ // skip clock_gettime
/* 114 */ // skip clock_getres
/* 115 */ // skip clock_nanosleep
/* 116 */ // skip syslog
/* 117 */ // skip ptrace
/* 118 */ // skip sched_setparam
/* 119 */ // skip sched_setscheduler
/* 120 */ // skip sched_getscheduler
/* 121 */ // skip sched_getparam
/* 122 */ // skip sched_setaffinity
/* 123 */ // skip sched_getaffinity
/* 124 */ // skip sched_yield
/* 125 */ // skip sched_get_priority_max
/* 126 */ // skip sched_get_priority_min
/* 127 */ // skip sched_rr_get_interval
/* 128 */ // skip restart_syscall
/* 129 */ // skip kill
/* 130 */ // skip tkill
/* 131 */ // skip tgkill
/* 132 */ // skip sigaltstack
/* 133 */ // skip rt_sigsuspend
/* 134 */ // skip rt_sigaction
/* 135 */ // skip rt_sigprocmask
/* 136 */ // skip rt_sigpending
/* 137 */ // skip rt_sigtimedwait
/* 138 */ // skip rt_sigqueueinfo
/* 139 */ // skip rt_sigreturn
/* 140 */ // skip setpriority
/* 141 */ // skip getpriority
/* 142 */ // skip reboot
/* 143 */ // skip setregid
/* 144 */ // skip setgid
/* 145 */ // skip setreuid
/* 146 */ // skip setuid
/* 147 */ // skip setresuid
/* 148 */ // skip getresuid
/* 149 */ // skip setresgid
/* 150 */ // skip getresgid
/* 151 */ // skip setfsuid
/* 152 */ // skip setfsgid
/* 153 */ // skip times
/* 154 */ // skip setpgid
/* 155 */ // skip getpgid
/* 156 */ // skip getsid
/* 157 */ // skip setsid
/* 158 */ // skip getgroups
/* 159 */ // skip setgroups
/* 160 */ // skip uname
/* 161 */ // skip sethostname
/* 162 */ // skip setdomainname
/* 163 */ // skip getrlimit
/* 164 */ // skip setrlimit
/* 165 */ // skip getrusage
/* 166 */ TRACE(umask);
/* 167 */ // skip prctl
/* 168 */ // skip getcpu
/* 169 */ // skip gettimeofday
/* 170 */ // skip settimeofday
/* 171 */ // skip adjtimex
/* 172 */ // skip getpid
/* 173 */ // skip getppid
/* 174 */ // skip getuid
/* 175 */ // skip geteuid
/* 176 */ // skip getgid
/* 177 */ // skip getegid
/* 178 */ // skip gettid
/* 179 */ // skip sysinfo
/* 180 */ // skip mq_open
/* 181 */ // skip mq_unlink
/* 182 */ // skip mq_timedsend
/* 183 */ // skip mq_timedreceive
/* 184 */ // skip mq_notify
/* 185 */ // skip mq_getsetattr
/* 186 */ // skip msgget
/* 187 */ // skip msgctl
/* 188 */ // skip msgrcv
/* 189 */ // skip msgsnd
/* 190 */ // skip semget
/* 191 */ // skip semctl
/* 192 */ // skip semtimedop
/* 193 */ // skip semop
/* 194 */ // skip shmget
/* 195 */ // skip shmctl
/* 196 */ // skip shmat
/* 197 */ // skip shmdt
/* 198 */ TRACE(socket);
/* 199 */ TRACE(socketpair);
/* 200 */ // skip bind
/* 201 */ // skip listen
/* 202 */ // skip accept
/* 203 */ // skip connect
/* 204 */ // skip getsockname
/* 205 */ // skip getpeername
/* 206 */ TRACE(sendto);
/* 207 */ TRACE(recvfrom);
/* 208 */ // skip setsockopt
/* 209 */ // skip getsockopt
/* 210 */ // skip shutdown
/* 211 */ TRACE(sendmsg);
/* 212 */ TRACE(recvmsg);
/* 213 */ // skip readahead
/* 214 */ // skip brk
/* 215 */ // skip munmap
/* 216 */ // skip mremap
/* 217 */ // skip add_key
/* 218 */ // skip request_key
/* 219 */ // skip keyctl
/* 220 */ // skip clone
/* 221 */ TRACE(execve);
/* 222 */ TRACE(mmap);
/* 223 */ // skip fadvise64
/* 224 */ // skip swapon
/* 225 */ // skip swapoff
/* 226 */ // skip mprotect
/* 227 */ // skip msync
/* 228 */ // skip mlock
/* 229 */ // skip munlock
/* 230 */ // skip mlockall
/* 231 */ // skip munlockall
/* 232 */ // skip mincore
/* 233 */ // skip madvise
/* 234 */ // skip remap_file_pages
/* 235 */ // skip mbind
/* 236 */ // skip get_mempolicy
/* 237 */ // skip set_mempolicy
/* 238 */ // skip migrate_pages
/* 239 */ // skip move_pages
/* 240 */ // skip rt_tgsigqueueinfo
/* 241 */ // skip perf_event_open
/* 242 */ // skip accept4
/* 243 */ // skip recvmmsg
/* 244 */ // skip arch_specific_syscall
/* 260 */ TRACE(wait4);
/* 261 */ // skip prlimit64
/* 262 */ // skip fanotify_init
/* 263 */ // skip fanotify_mark
/* 264 */ // skip name_to_handle_at
/* 265 */ // skip open_by_handle_at
/* 266 */ // skip clock_adjtime
/* 267 */ // skip syncfs
/* 268 */ // skip setns
/* 269 */ // skip sendmmsg
/* 270 */ // skip process_vm_readv
/* 271 */ // skip process_vm_writev
/* 272 */ // skip kcmp
/* 273 */ // skip finit_module
/* 274 */ // skip sched_setattr
/* 275 */ // skip sched_getattr
/* 276 */ TRACE(renameat2);
/* 277 */ // skip seccomp
/* 278 */ // skip getrandom
/* 279 */ // skip memfd_create
/* 280 */ // skip bpf
/* 281 */ TRACE(execveat);
/* 282 */ // skip userfaultfd
/* 283 */ // skip membarrier
/* 284 */ // skip mlock2
/* 285 */ TRACE(copy_file_range);
/* 286 */ TRACE(preadv2);
/* 287 */ TRACE(pwritev2);
/* 288 */ // skip pkey_mprotect
/* 289 */ // skip pkey_alloc
/* 290 */ // skip pkey_free
/* 291 */ TRACE(statx);
/* 292 */ // skip io_pgetevents
/* 293 */ // skip rseq
/* 294 */ // skip kexec_file_load
/* 403 */ // skip clock_gettime64
/* 404 */ // skip clock_settime64
/* 405 */ // skip clock_adjtime64
/* 406 */ // skip clock_getres_time64
/* 407 */ // skip clock_nanosleep_time64
/* 408 */ // skip timer_gettime64
/* 409 */ // skip timer_settime64
/* 410 */ // skip timerfd_gettime64
/* 411 */ // skip timerfd_settime64
/* 412 */ // skip utimensat_time64
/* 413 */ // skip pselect6_time64
/* 414 */ // skip ppoll_time64
/* 416 */ // skip io_pgetevents_time64
/* 417 */ // skip recvmmsg_time64
/* 418 */ // skip mq_timedsend_time64
/* 419 */ // skip mq_timedreceive_time64
/* 420 */ // skip semtimedop_time64
/* 421 */ // skip rt_sigtimedwait_time64
/* 422 */ // skip futex_time64
/* 423 */ // skip sched_rr_get_interval_time64
/* 424 */ // skip pidfd_send_signal
/* 425 */ // skip io_uring_setup
/* 426 */ // skip io_uring_enter
/* 427 */ // skip io_uring_register
/* 428 */ // skip open_tree
/* 429 */ // skip move_mount
/* 430 */ // skip fsopen
/* 431 */ // skip fsconfig
/* 432 */ // skip fsmount
/* 433 */ // skip fspick
/* 434 */ // skip pidfd_open
/* 435 */ // skip clone3
