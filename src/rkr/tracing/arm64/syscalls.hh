#pragma once

#if !defined(TRACE)
#error "syscalls.hh was included without the TRACE macro"
#endif

/* 000 */  // skip io_setup (__NR_io_setup)
/* 001 */  // skip io_destroy (__NR_io_destroy)
/* 002 */  // skip io_submit (__NR_io_submit)
/* 003 */  // skip io_cancel (__NR_io_cancel)
/* 004 */  // skip io_getevents (__NR_io_getevents)
/* 005 */  // skip setxattr (__NR_setxattr)
/* 006 */  // skip lsetxattr (__NR_lsetxattr)
/* 007 */  // skip fsetxattr (__NR_fsetxattr)
/* 008 */  // skip getxattr (__NR_getxattr)
/* 009 */  // skip lgetxattr (__NR_lgetxattr)
/* 010 */  // skip fgetxattr (__NR_fgetxattr)
/* 011 */  // skip listxattr (__NR_listxattr)
/* 012 */  // skip llistxattr (__NR_llistxattr)
/* 013 */  // skip flistxattr (__NR_flistxattr)
/* 014 */  // skip removexattr (__NR_removexattr)
/* 015 */  // skip lremovexattr (__NR_lremovexattr)
/* 016 */  // skip fremovexattr (__NR_fremovexattr)
/* 017 */  // skip getcwd (__NR_getcwd)
/* 018 */  // skip lookup_dcookie (__NR_lookup_dcookie)
/* 019 */  // skip eventfd2 (__NR_eventfd2)
/* 020 */  // skip epoll_create1 (__NR_epoll_create1)
/* 021 */  // skip epoll_ctl (__NR_epoll_ctl)
/* 022 */  // skip epoll_pwait (__NR_epoll_pwait)
/* 023 */ TRACE(__NR_dup, dup);
/* 024 */ TRACE(__NR_dup3, dup3);
/* 025 */ TRACE(__NR3264_fcntl, fcntl);
/* 026 */  // skip inotify_init1 (__NR_inotify_init1)
/* 027 */  // skip inotify_add_watch (__NR_inotify_add_watch)
/* 028 */  // skip inotify_rm_watch (__NR_inotify_rm_watch)
/* 029 */  // skip ioctl (__NR_ioctl)
/* 030 */  // skip ioprio_set (__NR_ioprio_set)
/* 031 */  // skip ioprio_get (__NR_ioprio_get)
/* 032 */  // skip flock (__NR_flock)
/* 033 */ TRACE(__NR_mknodat, mknodat);
/* 034 */ TRACE(__NR_mkdirat, mkdirat);
/* 035 */ TRACE(__NR_unlinkat, unlinkat);
/* 036 */ TRACE(__NR_symlinkat, symlinkat);
/* 037 */ TRACE(__NR_linkat, linkat);
/* 038 */ TRACE(__NR_renameat, renameat);
/* 039 */  // skip umount2 (__NR_umount2)
/* 040 */  // skip mount (__NR_mount)
/* 041 */ TRACE(__NR_pivot_root, pivot_root);
/* 042 */  // skip nfsservctl (__NR_nfsservctl)
/* 043 */  // skip statfs (__NR3264_statfs)
/* 044 */  // skip fstatfs (__NR3264_fstatfs)
/* 045 */ TRACE(__NR3264_truncate, truncate);
/* 046 */ TRACE(__NR3264_ftruncate, ftruncate);
/* 047 */  // skip fallocate (__NR_fallocate)
/* 048 */ TRACE(__NR_faccessat, faccessat);
/* 049 */ TRACE(__NR_chdir, chdir);
/* 050 */ TRACE(__NR_fchdir, fchdir);
/* 051 */ TRACE(__NR_chroot, chroot);
/* 052 */ TRACE(__NR_fchmod, fchmod);
/* 053 */ TRACE(__NR_fchmodat, fchmodat);
/* 054 */ TRACE(__NR_fchownat, fchownat);
/* 055 */ TRACE(__NR_fchown, fchown);
/* 056 */ TRACE(__NR_openat, openat);
/* 057 */ TRACE(__NR_close, close);
/* 058 */  // skip vhangup (__NR_vhangup)
/* 059 */ TRACE(__NR_pipe2, pipe2);
/* 060 */  // skip quotactl (__NR_quotactl)
/* 061 */ TRACE(__NR_getdents64, getdents64);
/* 062 */  // skip lseek (__NR3264_lseek)
/* 063 */ TRACE(__NR_read, read);
/* 064 */ TRACE(__NR_write, write);
/* 065 */ TRACE(__NR_readv, readv);
/* 066 */ TRACE(__NR_writev, writev);
/* 067 */ TRACE(__NR_pread64, pread64);
/* 068 */ TRACE(__NR_pwrite64, pwrite64);
/* 069 */ TRACE(__NR_preadv, preadv);
/* 070 */ TRACE(__NR_pwritev, pwritev);
/* 071 */ TRACE(__NR3264_sendfile, sendfile);
/* 072 */  // skip pselect6 (__NR_pselect6)
/* 073 */  // skip ppoll (__NR_ppoll)
/* 074 */  // skip signalfd4 (__NR_signalfd4)
/* 075 */ TRACE(__NR_vmsplice, vmsplice);
/* 076 */ TRACE(__NR_splice, splice);
/* 077 */ TRACE(__NR_tee, tee);
/* 078 */ TRACE(__NR_readlinkat, readlinkat);
/* 079 */ TRACE(__NR3264_fstatat, fstatat);
/* 080 */ TRACE(__NR3264_fstat, fstat);
/* 081 */  // skip sync (__NR_sync)
/* 082 */  // skip fsync (__NR_fsync)
/* 083 */  // skip fdatasync (__NR_fdatasync)
/* 084 */  // skip sync_file_range2 (__NR_sync_file_range2)
/* 084 */  // skip sync_file_range (__NR_sync_file_range)
/* 085 */  // skip timerfd_create (__NR_timerfd_create)
/* 086 */  // skip timerfd_settime (__NR_timerfd_settime)
/* 087 */  // skip timerfd_gettime (__NR_timerfd_gettime)
/* 088 */  // skip utimensat (__NR_utimensat)
/* 089 */  // skip acct (__NR_acct)
/* 090 */  // skip capget (__NR_capget)
/* 091 */  // skip capset (__NR_capset)
/* 092 */  // skip personality (__NR_personality)
/* 093 */  // skip exit (__NR_exit)
/* 094 */  // skip exit_group (__NR_exit_group)
/* 095 */ TRACE(__NR_waitid, waitid);
/* 096 */  // skip set_tid_address (__NR_set_tid_address)
/* 097 */  // skip unshare (__NR_unshare)
/* 098 */  // skip futex (__NR_futex)
/* 099 */  // skip set_robust_list (__NR_set_robust_list)
/* 100 */  // skip get_robust_list (__NR_get_robust_list)
/* 101 */  // skip nanosleep (__NR_nanosleep)
/* 102 */  // skip getitimer (__NR_getitimer)
/* 103 */  // skip setitimer (__NR_setitimer)
/* 104 */  // skip kexec_load (__NR_kexec_load)
/* 105 */  // skip init_module (__NR_init_module)
/* 106 */  // skip delete_module (__NR_delete_module)
/* 107 */  // skip timer_create (__NR_timer_create)
/* 108 */  // skip timer_gettime (__NR_timer_gettime)
/* 109 */  // skip timer_getoverrun (__NR_timer_getoverrun)
/* 110 */  // skip timer_settime (__NR_timer_settime)
/* 111 */  // skip timer_delete (__NR_timer_delete)
/* 112 */  // skip clock_settime (__NR_clock_settime)
/* 113 */  // skip clock_gettime (__NR_clock_gettime)
/* 114 */  // skip clock_getres (__NR_clock_getres)
/* 115 */  // skip clock_nanosleep (__NR_clock_nanosleep)
/* 116 */  // skip syslog (__NR_syslog)
/* 117 */  // skip ptrace (__NR_ptrace)
/* 118 */  // skip sched_setparam (__NR_sched_setparam)
/* 119 */  // skip sched_setscheduler (__NR_sched_setscheduler)
/* 120 */  // skip sched_getscheduler (__NR_sched_getscheduler)
/* 121 */  // skip sched_getparam (__NR_sched_getparam)
/* 122 */  // skip sched_setaffinity (__NR_sched_setaffinity)
/* 123 */  // skip sched_getaffinity (__NR_sched_getaffinity)
/* 124 */  // skip sched_yield (__NR_sched_yield)
/* 125 */  // skip sched_get_priority_max (__NR_sched_get_priority_max)
/* 126 */  // skip sched_get_priority_min (__NR_sched_get_priority_min)
/* 127 */  // skip sched_rr_get_interval (__NR_sched_rr_get_interval)
/* 128 */  // skip restart_syscall (__NR_restart_syscall)
/* 129 */  // skip kill (__NR_kill)
/* 130 */  // skip tkill (__NR_tkill)
/* 131 */  // skip tgkill (__NR_tgkill)
/* 132 */  // skip sigaltstack (__NR_sigaltstack)
/* 133 */  // skip rt_sigsuspend (__NR_rt_sigsuspend)
/* 134 */  // skip rt_sigaction (__NR_rt_sigaction)
/* 135 */  // skip rt_sigprocmask (__NR_rt_sigprocmask)
/* 136 */  // skip rt_sigpending (__NR_rt_sigpending)
/* 137 */  // skip rt_sigtimedwait (__NR_rt_sigtimedwait)
/* 138 */  // skip rt_sigqueueinfo (__NR_rt_sigqueueinfo)
/* 139 */  // skip rt_sigreturn (__NR_rt_sigreturn)
/* 140 */  // skip setpriority (__NR_setpriority)
/* 141 */  // skip getpriority (__NR_getpriority)
/* 142 */  // skip reboot (__NR_reboot)
/* 143 */  // skip setregid (__NR_setregid)
/* 144 */  // skip setgid (__NR_setgid)
/* 145 */  // skip setreuid (__NR_setreuid)
/* 146 */  // skip setuid (__NR_setuid)
/* 147 */  // skip setresuid (__NR_setresuid)
/* 148 */  // skip getresuid (__NR_getresuid)
/* 149 */  // skip setresgid (__NR_setresgid)
/* 150 */  // skip getresgid (__NR_getresgid)
/* 151 */  // skip setfsuid (__NR_setfsuid)
/* 152 */  // skip setfsgid (__NR_setfsgid)
/* 153 */  // skip times (__NR_times)
/* 154 */  // skip setpgid (__NR_setpgid)
/* 155 */  // skip getpgid (__NR_getpgid)
/* 156 */  // skip getsid (__NR_getsid)
/* 157 */  // skip setsid (__NR_setsid)
/* 158 */  // skip getgroups (__NR_getgroups)
/* 159 */  // skip setgroups (__NR_setgroups)
/* 160 */  // skip uname (__NR_uname)
/* 161 */  // skip sethostname (__NR_sethostname)
/* 162 */  // skip setdomainname (__NR_setdomainname)
/* 163 */  // skip getrlimit (__NR_getrlimit)
/* 164 */  // skip setrlimit (__NR_setrlimit)
/* 165 */  // skip getrusage (__NR_getrusage)
/* 166 */ TRACE(__NR_umask, umask);
/* 167 */  // skip prctl (__NR_prctl)
/* 168 */  // skip getcpu (__NR_getcpu)
/* 169 */  // skip gettimeofday (__NR_gettimeofday)
/* 170 */  // skip settimeofday (__NR_settimeofday)
/* 171 */  // skip adjtimex (__NR_adjtimex)
/* 172 */  // skip getpid (__NR_getpid)
/* 173 */  // skip getppid (__NR_getppid)
/* 174 */  // skip getuid (__NR_getuid)
/* 175 */  // skip geteuid (__NR_geteuid)
/* 176 */  // skip getgid (__NR_getgid)
/* 177 */  // skip getegid (__NR_getegid)
/* 178 */  // skip gettid (__NR_gettid)
/* 179 */  // skip sysinfo (__NR_sysinfo)
/* 180 */  // skip mq_open (__NR_mq_open)
/* 181 */  // skip mq_unlink (__NR_mq_unlink)
/* 182 */  // skip mq_timedsend (__NR_mq_timedsend)
/* 183 */  // skip mq_timedreceive (__NR_mq_timedreceive)
/* 184 */  // skip mq_notify (__NR_mq_notify)
/* 185 */  // skip mq_getsetattr (__NR_mq_getsetattr)
/* 186 */  // skip msgget (__NR_msgget)
/* 187 */  // skip msgctl (__NR_msgctl)
/* 188 */  // skip msgrcv (__NR_msgrcv)
/* 189 */  // skip msgsnd (__NR_msgsnd)
/* 190 */  // skip semget (__NR_semget)
/* 191 */  // skip semctl (__NR_semctl)
/* 192 */  // skip semtimedop (__NR_semtimedop)
/* 193 */  // skip semop (__NR_semop)
/* 194 */  // skip shmget (__NR_shmget)
/* 195 */  // skip shmctl (__NR_shmctl)
/* 196 */  // skip shmat (__NR_shmat)
/* 197 */  // skip shmdt (__NR_shmdt)
/* 198 */ TRACE(__NR_socket, socket);
/* 199 */ TRACE(__NR_socketpair, socketpair);
/* 200 */  // skip bind (__NR_bind)
/* 201 */  // skip listen (__NR_listen)
/* 202 */ TRACE(__NR_accept, accept);
/* 203 */  // skip connect (__NR_connect)
/* 204 */  // skip getsockname (__NR_getsockname)
/* 205 */  // skip getpeername (__NR_getpeername)
/* 206 */ TRACE(__NR_sendto, sendto);
/* 207 */ TRACE(__NR_recvfrom, recvfrom);
/* 208 */  // skip setsockopt (__NR_setsockopt)
/* 209 */  // skip getsockopt (__NR_getsockopt)
/* 210 */  // skip shutdown (__NR_shutdown)
/* 211 */ TRACE(__NR_sendmsg, sendmsg);
/* 212 */ TRACE(__NR_recvmsg, recvmsg);
/* 213 */  // skip readahead (__NR_readahead)
/* 214 */  // skip brk (__NR_brk)
/* 215 */  // skip munmap (__NR_munmap)
/* 216 */  // skip mremap (__NR_mremap)
/* 217 */  // skip add_key (__NR_add_key)
/* 218 */  // skip request_key (__NR_request_key)
/* 219 */  // skip keyctl (__NR_keyctl)
/* 220 */  // skip clone (__NR_clone)
/* 221 */ TRACE(__NR_execve, execve);
/* 222 */ TRACE(__NR3264_mmap, mmap);
/* 223 */  // skip fadvise64 (__NR3264_fadvise64)
/* 224 */  // skip swapon (__NR_swapon)
/* 225 */  // skip swapoff (__NR_swapoff)
/* 226 */  // skip mprotect (__NR_mprotect)
/* 227 */  // skip msync (__NR_msync)
/* 228 */  // skip mlock (__NR_mlock)
/* 229 */  // skip munlock (__NR_munlock)
/* 230 */  // skip mlockall (__NR_mlockall)
/* 231 */  // skip munlockall (__NR_munlockall)
/* 232 */  // skip mincore (__NR_mincore)
/* 233 */  // skip madvise (__NR_madvise)
/* 234 */  // skip remap_file_pages (__NR_remap_file_pages)
/* 235 */  // skip mbind (__NR_mbind)
/* 236 */  // skip get_mempolicy (__NR_get_mempolicy)
/* 237 */  // skip set_mempolicy (__NR_set_mempolicy)
/* 238 */  // skip migrate_pages (__NR_migrate_pages)
/* 239 */  // skip move_pages (__NR_move_pages)
/* 240 */  // skip rt_tgsigqueueinfo (__NR_rt_tgsigqueueinfo)
/* 241 */  // skip perf_event_open (__NR_perf_event_open)
/* 242 */ TRACE(__NR_accept4, accept4);
/* 243 */  // skip recvmmsg (__NR_recvmmsg)
/* 244 */  // skip arch_specific_syscall (__NR_arch_specific_syscall)
/* 260 */ TRACE(__NR_wait4, wait4);
/* 261 */  // skip prlimit64 (__NR_prlimit64)
/* 262 */  // skip fanotify_init (__NR_fanotify_init)
/* 263 */  // skip fanotify_mark (__NR_fanotify_mark)
/* 264 */  // skip name_to_handle_at (__NR_name_to_handle_at)
/* 265 */  // skip open_by_handle_at (__NR_open_by_handle_at)
/* 266 */  // skip clock_adjtime (__NR_clock_adjtime)
/* 267 */  // skip syncfs (__NR_syncfs)
/* 268 */  // skip setns (__NR_setns)
/* 269 */  // skip sendmmsg (__NR_sendmmsg)
/* 270 */  // skip process_vm_readv (__NR_process_vm_readv)
/* 271 */  // skip process_vm_writev (__NR_process_vm_writev)
/* 272 */  // skip kcmp (__NR_kcmp)
/* 273 */  // skip finit_module (__NR_finit_module)
/* 274 */  // skip sched_setattr (__NR_sched_setattr)
/* 275 */  // skip sched_getattr (__NR_sched_getattr)
/* 276 */ TRACE(__NR_renameat2, renameat2);
/* 277 */  // skip seccomp (__NR_seccomp)
/* 278 */  // skip getrandom (__NR_getrandom)
/* 279 */  // skip memfd_create (__NR_memfd_create)
/* 280 */  // skip bpf (__NR_bpf)
/* 281 */ TRACE(__NR_execveat, execveat);
/* 282 */  // skip userfaultfd (__NR_userfaultfd)
/* 283 */  // skip membarrier (__NR_membarrier)
/* 284 */  // skip mlock2 (__NR_mlock2)
/* 285 */ TRACE(__NR_copy_file_range, copy_file_range);
/* 286 */ TRACE(__NR_preadv2, preadv2);
/* 287 */ TRACE(__NR_pwritev2, pwritev2);
/* 288 */  // skip pkey_mprotect (__NR_pkey_mprotect)
/* 289 */  // skip pkey_alloc (__NR_pkey_alloc)
/* 290 */  // skip pkey_free (__NR_pkey_free)
/* 291 */ TRACE(__NR_statx, statx);
/* 292 */  // skip io_pgetevents (__NR_io_pgetevents)
/* 293 */  // skip rseq (__NR_rseq)
/* 294 */  // skip kexec_file_load (__NR_kexec_file_load)
/* 403 */  // skip clock_gettime64 (__NR_clock_gettime64)
/* 404 */  // skip clock_settime64 (__NR_clock_settime64)
/* 405 */  // skip clock_adjtime64 (__NR_clock_adjtime64)
/* 406 */  // skip clock_getres_time64 (__NR_clock_getres_time64)
/* 407 */  // skip clock_nanosleep_time64 (__NR_clock_nanosleep_time64)
/* 408 */  // skip timer_gettime64 (__NR_timer_gettime64)
/* 409 */  // skip timer_settime64 (__NR_timer_settime64)
/* 410 */  // skip timerfd_gettime64 (__NR_timerfd_gettime64)
/* 411 */  // skip timerfd_settime64 (__NR_timerfd_settime64)
/* 412 */  // skip utimensat_time64 (__NR_utimensat_time64)
/* 413 */  // skip pselect6_time64 (__NR_pselect6_time64)
/* 414 */  // skip ppoll_time64 (__NR_ppoll_time64)
/* 416 */  // skip io_pgetevents_time64 (__NR_io_pgetevents_time64)
/* 417 */  // skip recvmmsg_time64 (__NR_recvmmsg_time64)
/* 418 */  // skip mq_timedsend_time64 (__NR_mq_timedsend_time64)
/* 419 */  // skip mq_timedreceive_time64 (__NR_mq_timedreceive_time64)
/* 420 */  // skip semtimedop_time64 (__NR_semtimedop_time64)
/* 421 */  // skip rt_sigtimedwait_time64 (__NR_rt_sigtimedwait_time64)
/* 422 */  // skip futex_time64 (__NR_futex_time64)
/* 423 */  // skip sched_rr_get_interval_time64 (__NR_sched_rr_get_interval_time64)
/* 424 */  // skip pidfd_send_signal (__NR_pidfd_send_signal)
/* 425 */  // skip io_uring_setup (__NR_io_uring_setup)
/* 426 */  // skip io_uring_enter (__NR_io_uring_enter)
/* 427 */  // skip io_uring_register (__NR_io_uring_register)
/* 428 */  // skip open_tree (__NR_open_tree)
/* 429 */  // skip move_mount (__NR_move_mount)
/* 430 */  // skip fsopen (__NR_fsopen)
/* 431 */  // skip fsconfig (__NR_fsconfig)
/* 432 */  // skip fsmount (__NR_fsmount)
/* 433 */  // skip fspick (__NR_fspick)
/* 434 */  // skip pidfd_open (__NR_pidfd_open)
/* 435 */  // skip clone3 (__NR_clone3)
/* 436 */  // unrecognized close_range (__NR_close_range)
/* 437 */  // unrecognized openat2 (__NR_openat2)
/* 438 */  // unrecognized pidfd_getfd (__NR_pidfd_getfd)
/* 439 */  // unrecognized faccessat2 (__NR_faccessat2)
/* 440 */  // unrecognized process_madvise (__NR_process_madvise)
/* 441 */  // unrecognized epoll_pwait2 (__NR_epoll_pwait2)
/* 442 */  // unrecognized mount_setattr (__NR_mount_setattr)
/* 443 */  // unrecognized quotactl_fd (__NR_quotactl_fd)
/* 444 */  // unrecognized landlock_create_ruleset (__NR_landlock_create_ruleset)
/* 445 */  // unrecognized landlock_add_rule (__NR_landlock_add_rule)
/* 446 */  // unrecognized landlock_restrict_self (__NR_landlock_restrict_self)
/* 447 */  // unrecognized memfd_secret (__NR_memfd_secret)
/* 448 */  // unrecognized process_mrelease (__NR_process_mrelease)
