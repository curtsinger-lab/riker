#pragma once

#if !defined(TRACE)
#error "syscalls.hh was included without the TRACE macro"
#endif

/* 000 */ TRACE(__NR_read, read);
/* 001 */ TRACE(__NR_write, write);
/* 002 */ TRACE(__NR_open, open);
/* 003 */ TRACE(__NR_close, close);
/* 004 */ TRACE(__NR_stat, stat);
/* 005 */ TRACE(__NR_fstat, fstat);
/* 006 */ TRACE(__NR_lstat, lstat);
/* 007 */ // skip poll (__NR_poll)
/* 008 */ // skip lseek (__NR_lseek)
/* 009 */ TRACE(__NR_mmap, mmap);
/* 010 */ // skip mprotect (__NR_mprotect)
/* 011 */ // skip munmap (__NR_munmap)
/* 012 */ // skip brk (__NR_brk)
/* 013 */ // skip rt_sigaction (__NR_rt_sigaction)
/* 014 */ // skip rt_sigprocmask (__NR_rt_sigprocmask)
/* 015 */ // skip rt_sigreturn (__NR_rt_sigreturn)
/* 016 */ // skip ioctl (__NR_ioctl)
/* 017 */ TRACE(__NR_pread64, pread64);
/* 018 */ TRACE(__NR_pwrite64, pwrite64);
/* 019 */ TRACE(__NR_readv, readv);
/* 020 */ TRACE(__NR_writev, writev);
/* 021 */ TRACE(__NR_access, access);
/* 022 */ TRACE(__NR_pipe, pipe);
/* 023 */ // skip select (__NR_select)
/* 024 */ // skip sched_yield (__NR_sched_yield)
/* 025 */ // skip mremap (__NR_mremap)
/* 026 */ // skip msync (__NR_msync)
/* 027 */ // skip mincore (__NR_mincore)
/* 028 */ // skip madvise (__NR_madvise)
/* 029 */ // skip shmget (__NR_shmget)
/* 030 */ // skip shmat (__NR_shmat)
/* 031 */ // skip shmctl (__NR_shmctl)
/* 032 */ TRACE(__NR_dup, dup);
/* 033 */ TRACE(__NR_dup2, dup2);
/* 034 */ // skip pause (__NR_pause)
/* 035 */ // skip nanosleep (__NR_nanosleep)
/* 036 */ // skip getitimer (__NR_getitimer)
/* 037 */ // skip alarm (__NR_alarm)
/* 038 */ // skip setitimer (__NR_setitimer)
/* 039 */ // skip getpid (__NR_getpid)
/* 040 */ TRACE(__NR_sendfile, sendfile);
/* 041 */ TRACE(__NR_socket, socket);
/* 042 */ // skip connect (__NR_connect)
/* 043 */ // skip accept (__NR_accept)
/* 044 */ TRACE(__NR_sendto, sendto);
/* 045 */ TRACE(__NR_recvfrom, recvfrom);
/* 046 */ TRACE(__NR_sendmsg, sendmsg);
/* 047 */ TRACE(__NR_recvmsg, recvmsg);
/* 048 */ // skip shutdown (__NR_shutdown)
/* 049 */ // skip bind (__NR_bind)
/* 050 */ // skip listen (__NR_listen)
/* 051 */ // skip getsockname (__NR_getsockname)
/* 052 */ // skip getpeername (__NR_getpeername)
/* 053 */ TRACE(__NR_socketpair, socketpair);
/* 054 */ // skip setsockopt (__NR_setsockopt)
/* 055 */ // skip getsockopt (__NR_getsockopt)
/* 056 */ // skip clone (__NR_clone)
/* 057 */ // skip fork (__NR_fork)
/* 058 */ // skip vfork (__NR_vfork)
/* 059 */ TRACE(__NR_execve, execve);
/* 060 */ // skip exit (__NR_exit)
/* 061 */ TRACE(__NR_wait4, wait4);
/* 062 */ // skip kill (__NR_kill)
/* 063 */ // skip uname (__NR_uname)
/* 064 */ // skip semget (__NR_semget)
/* 065 */ // skip semop (__NR_semop)
/* 066 */ // skip semctl (__NR_semctl)
/* 067 */ // skip shmdt (__NR_shmdt)
/* 068 */ // skip msgget (__NR_msgget)
/* 069 */ // skip msgsnd (__NR_msgsnd)
/* 070 */ // skip msgrcv (__NR_msgrcv)
/* 071 */ // skip msgctl (__NR_msgctl)
/* 072 */ TRACE(__NR_fcntl, fcntl);
/* 073 */ // skip flock (__NR_flock)
/* 074 */ // skip fsync (__NR_fsync)
/* 075 */ // skip fdatasync (__NR_fdatasync)
/* 076 */ TRACE(__NR_truncate, truncate);
/* 077 */ TRACE(__NR_ftruncate, ftruncate);
/* 078 */ TRACE(__NR_getdents, getdents);
/* 079 */ // skip getcwd (__NR_getcwd)
/* 080 */ TRACE(__NR_chdir, chdir);
/* 081 */ TRACE(__NR_fchdir, fchdir);
/* 082 */ TRACE(__NR_rename, rename);
/* 083 */ TRACE(__NR_mkdir, mkdir);
/* 084 */ TRACE(__NR_rmdir, rmdir);
/* 085 */ TRACE(__NR_creat, creat);
/* 086 */ TRACE(__NR_link, link);
/* 087 */ TRACE(__NR_unlink, unlink);
/* 088 */ TRACE(__NR_symlink, symlink);
/* 089 */ TRACE(__NR_readlink, readlink);
/* 090 */ TRACE(__NR_chmod, chmod);
/* 091 */ TRACE(__NR_fchmod, fchmod);
/* 092 */ TRACE(__NR_chown, chown);
/* 093 */ TRACE(__NR_fchown, fchown);
/* 094 */ TRACE(__NR_lchown, lchown);
/* 095 */ TRACE(__NR_umask, umask);
/* 096 */ // skip gettimeofday (__NR_gettimeofday)
/* 097 */ // skip getrlimit (__NR_getrlimit)
/* 098 */ // skip getrusage (__NR_getrusage)
/* 099 */ // skip sysinfo (__NR_sysinfo)
/* 100 */ // skip times (__NR_times)
/* 101 */ // skip ptrace (__NR_ptrace)
/* 102 */ // skip getuid (__NR_getuid)
/* 103 */ // skip syslog (__NR_syslog)
/* 104 */ // skip getgid (__NR_getgid)
/* 105 */ // skip setuid (__NR_setuid)
/* 106 */ // skip setgid (__NR_setgid)
/* 107 */ // skip geteuid (__NR_geteuid)
/* 108 */ // skip getegid (__NR_getegid)
/* 109 */ // skip setpgid (__NR_setpgid)
/* 110 */ // skip getppid (__NR_getppid)
/* 111 */ // skip getpgrp (__NR_getpgrp)
/* 112 */ // skip setsid (__NR_setsid)
/* 113 */ // skip setreuid (__NR_setreuid)
/* 114 */ // skip setregid (__NR_setregid)
/* 115 */ // skip getgroups (__NR_getgroups)
/* 116 */ // skip setgroups (__NR_setgroups)
/* 117 */ // skip setresuid (__NR_setresuid)
/* 118 */ // skip getresuid (__NR_getresuid)
/* 119 */ // skip setresgid (__NR_setresgid)
/* 120 */ // skip getresgid (__NR_getresgid)
/* 121 */ // skip getpgid (__NR_getpgid)
/* 122 */ // skip setfsuid (__NR_setfsuid)
/* 123 */ // skip setfsgid (__NR_setfsgid)
/* 124 */ // skip getsid (__NR_getsid)
/* 125 */ // skip capget (__NR_capget)
/* 126 */ // skip capset (__NR_capset)
/* 127 */ // skip rt_sigpending (__NR_rt_sigpending)
/* 128 */ // skip rt_sigtimedwait (__NR_rt_sigtimedwait)
/* 129 */ // skip rt_sigqueueinfo (__NR_rt_sigqueueinfo)
/* 130 */ // skip rt_sigsuspend (__NR_rt_sigsuspend)
/* 131 */ // skip sigaltstack (__NR_sigaltstack)
/* 132 */ // skip utime (__NR_utime)
/* 133 */ TRACE(__NR_mknod, mknod);
/* 134 */ // skip uselib (__NR_uselib)
/* 135 */ // skip personality (__NR_personality)
/* 136 */ // skip ustat (__NR_ustat)
/* 137 */ // skip statfs (__NR_statfs)
/* 138 */ // skip fstatfs (__NR_fstatfs)
/* 139 */ // skip sysfs (__NR_sysfs)
/* 140 */ // skip getpriority (__NR_getpriority)
/* 141 */ // skip setpriority (__NR_setpriority)
/* 142 */ // skip sched_setparam (__NR_sched_setparam)
/* 143 */ // skip sched_getparam (__NR_sched_getparam)
/* 144 */ // skip sched_setscheduler (__NR_sched_setscheduler)
/* 145 */ // skip sched_getscheduler (__NR_sched_getscheduler)
/* 146 */ // skip sched_get_priority_max (__NR_sched_get_priority_max)
/* 147 */ // skip sched_get_priority_min (__NR_sched_get_priority_min)
/* 148 */ // skip sched_rr_get_interval (__NR_sched_rr_get_interval)
/* 149 */ // skip mlock (__NR_mlock)
/* 150 */ // skip munlock (__NR_munlock)
/* 151 */ // skip mlockall (__NR_mlockall)
/* 152 */ // skip munlockall (__NR_munlockall)
/* 153 */ // skip vhangup (__NR_vhangup)
/* 154 */ // skip modify_ldt (__NR_modify_ldt)
/* 155 */ TRACE(__NR_pivot_root, pivot_root);
/* 156 */ // skip _sysctl (__NR__sysctl)
/* 157 */ // skip prctl (__NR_prctl)
/* 158 */ // skip arch_prctl (__NR_arch_prctl)
/* 159 */ // skip adjtimex (__NR_adjtimex)
/* 160 */ // skip setrlimit (__NR_setrlimit)
/* 161 */ TRACE(__NR_chroot, chroot);
/* 162 */ // skip sync (__NR_sync)
/* 163 */ // skip acct (__NR_acct)
/* 164 */ // skip settimeofday (__NR_settimeofday)
/* 165 */ // skip mount (__NR_mount)
/* 166 */ // skip umount2 (__NR_umount2)
/* 167 */ // skip swapon (__NR_swapon)
/* 168 */ // skip swapoff (__NR_swapoff)
/* 169 */ // skip reboot (__NR_reboot)
/* 170 */ // skip sethostname (__NR_sethostname)
/* 171 */ // skip setdomainname (__NR_setdomainname)
/* 172 */ // skip iopl (__NR_iopl)
/* 173 */ // skip ioperm (__NR_ioperm)
/* 174 */ // skip create_module (__NR_create_module)
/* 175 */ // skip init_module (__NR_init_module)
/* 176 */ // skip delete_module (__NR_delete_module)
/* 177 */ // skip get_kernel_syms (__NR_get_kernel_syms)
/* 178 */ // skip query_module (__NR_query_module)
/* 179 */ // skip quotactl (__NR_quotactl)
/* 180 */ // skip nfsservctl (__NR_nfsservctl)
/* 181 */ // skip getpmsg (__NR_getpmsg)
/* 182 */ // skip putpmsg (__NR_putpmsg)
/* 183 */ // skip afs_syscall (__NR_afs_syscall)
/* 184 */ // skip tuxcall (__NR_tuxcall)
/* 185 */ // skip security (__NR_security)
/* 186 */ // skip gettid (__NR_gettid)
/* 187 */ // skip readahead (__NR_readahead)
/* 188 */ // skip setxattr (__NR_setxattr)
/* 189 */ // skip lsetxattr (__NR_lsetxattr)
/* 190 */ // skip fsetxattr (__NR_fsetxattr)
/* 191 */ // skip getxattr (__NR_getxattr)
/* 192 */ // skip lgetxattr (__NR_lgetxattr)
/* 193 */ // skip fgetxattr (__NR_fgetxattr)
/* 194 */ // skip listxattr (__NR_listxattr)
/* 195 */ // skip llistxattr (__NR_llistxattr)
/* 196 */ // skip flistxattr (__NR_flistxattr)
/* 197 */ // skip removexattr (__NR_removexattr)
/* 198 */ // skip lremovexattr (__NR_lremovexattr)
/* 199 */ // skip fremovexattr (__NR_fremovexattr)
/* 200 */ // skip tkill (__NR_tkill)
/* 201 */ // skip time (__NR_time)
/* 202 */ // skip futex (__NR_futex)
/* 203 */ // skip sched_setaffinity (__NR_sched_setaffinity)
/* 204 */ // skip sched_getaffinity (__NR_sched_getaffinity)
/* 205 */ // skip set_thread_area (__NR_set_thread_area)
/* 206 */ // skip io_setup (__NR_io_setup)
/* 207 */ // skip io_destroy (__NR_io_destroy)
/* 208 */ // skip io_getevents (__NR_io_getevents)
/* 209 */ // skip io_submit (__NR_io_submit)
/* 210 */ // skip io_cancel (__NR_io_cancel)
/* 211 */ // skip get_thread_area (__NR_get_thread_area)
/* 212 */ // skip lookup_dcookie (__NR_lookup_dcookie)
/* 213 */ // skip epoll_create (__NR_epoll_create)
/* 214 */ // skip epoll_ctl_old (__NR_epoll_ctl_old)
/* 215 */ // skip epoll_wait_old (__NR_epoll_wait_old)
/* 216 */ // skip remap_file_pages (__NR_remap_file_pages)
/* 217 */ TRACE(__NR_getdents64, getdents64);
/* 218 */ // skip set_tid_address (__NR_set_tid_address)
/* 219 */ // skip restart_syscall (__NR_restart_syscall)
/* 220 */ // skip semtimedop (__NR_semtimedop)
/* 221 */ // skip fadvise64 (__NR_fadvise64)
/* 222 */ // skip timer_create (__NR_timer_create)
/* 223 */ // skip timer_settime (__NR_timer_settime)
/* 224 */ // skip timer_gettime (__NR_timer_gettime)
/* 225 */ // skip timer_getoverrun (__NR_timer_getoverrun)
/* 226 */ // skip timer_delete (__NR_timer_delete)
/* 227 */ // skip clock_settime (__NR_clock_settime)
/* 228 */ // skip clock_gettime (__NR_clock_gettime)
/* 229 */ // skip clock_getres (__NR_clock_getres)
/* 230 */ // skip clock_nanosleep (__NR_clock_nanosleep)
/* 231 */ // skip exit_group (__NR_exit_group)
/* 232 */ // skip epoll_wait (__NR_epoll_wait)
/* 233 */ // skip epoll_ctl (__NR_epoll_ctl)
/* 234 */ // skip tgkill (__NR_tgkill)
/* 235 */ // skip utimes (__NR_utimes)
/* 236 */ // skip vserver (__NR_vserver)
/* 237 */ // skip mbind (__NR_mbind)
/* 238 */ // skip set_mempolicy (__NR_set_mempolicy)
/* 239 */ // skip get_mempolicy (__NR_get_mempolicy)
/* 240 */ // skip mq_open (__NR_mq_open)
/* 241 */ // skip mq_unlink (__NR_mq_unlink)
/* 242 */ // skip mq_timedsend (__NR_mq_timedsend)
/* 243 */ // skip mq_timedreceive (__NR_mq_timedreceive)
/* 244 */ // skip mq_notify (__NR_mq_notify)
/* 245 */ // skip mq_getsetattr (__NR_mq_getsetattr)
/* 246 */ // skip kexec_load (__NR_kexec_load)
/* 247 */ TRACE(__NR_waitid, waitid);
/* 248 */ // skip add_key (__NR_add_key)
/* 249 */ // skip request_key (__NR_request_key)
/* 250 */ // skip keyctl (__NR_keyctl)
/* 251 */ // skip ioprio_set (__NR_ioprio_set)
/* 252 */ // skip ioprio_get (__NR_ioprio_get)
/* 253 */ // skip inotify_init (__NR_inotify_init)
/* 254 */ // skip inotify_add_watch (__NR_inotify_add_watch)
/* 255 */ // skip inotify_rm_watch (__NR_inotify_rm_watch)
/* 256 */ // skip migrate_pages (__NR_migrate_pages)
/* 257 */ TRACE(__NR_openat, openat);
/* 258 */ TRACE(__NR_mkdirat, mkdirat);
/* 259 */ TRACE(__NR_mknodat, mknodat);
/* 260 */ TRACE(__NR_fchownat, fchownat);
/* 261 */ // skip futimesat (__NR_futimesat)
/* 262 */ TRACE(__NR_newfstatat, newfstatat);
/* 263 */ TRACE(__NR_unlinkat, unlinkat);
/* 264 */ TRACE(__NR_renameat, renameat);
/* 265 */ TRACE(__NR_linkat, linkat);
/* 266 */ TRACE(__NR_symlinkat, symlinkat);
/* 267 */ TRACE(__NR_readlinkat, readlinkat);
/* 268 */ TRACE(__NR_fchmodat, fchmodat);
/* 269 */ TRACE(__NR_faccessat, faccessat);
/* 270 */ // skip pselect6 (__NR_pselect6)
/* 271 */ // skip ppoll (__NR_ppoll)
/* 272 */ // skip unshare (__NR_unshare)
/* 273 */ // skip set_robust_list (__NR_set_robust_list)
/* 274 */ // skip get_robust_list (__NR_get_robust_list)
/* 275 */ TRACE(__NR_splice, splice);
/* 276 */ TRACE(__NR_tee, tee);
/* 277 */ // skip sync_file_range (__NR_sync_file_range)
/* 278 */ TRACE(__NR_vmsplice, vmsplice);
/* 279 */ // skip move_pages (__NR_move_pages)
/* 280 */ // skip utimensat (__NR_utimensat)
/* 281 */ // skip epoll_pwait (__NR_epoll_pwait)
/* 282 */ // skip signalfd (__NR_signalfd)
/* 283 */ // skip timerfd_create (__NR_timerfd_create)
/* 284 */ // skip eventfd (__NR_eventfd)
/* 285 */ // skip fallocate (__NR_fallocate)
/* 286 */ // skip timerfd_settime (__NR_timerfd_settime)
/* 287 */ // skip timerfd_gettime (__NR_timerfd_gettime)
/* 288 */ // skip accept4 (__NR_accept4)
/* 289 */ // skip signalfd4 (__NR_signalfd4)
/* 290 */ // skip eventfd2 (__NR_eventfd2)
/* 291 */ // skip epoll_create1 (__NR_epoll_create1)
/* 292 */ TRACE(__NR_dup3, dup3);
/* 293 */ TRACE(__NR_pipe2, pipe2);
/* 294 */ // skip inotify_init1 (__NR_inotify_init1)
/* 295 */ TRACE(__NR_preadv, preadv);
/* 296 */ TRACE(__NR_pwritev, pwritev);
/* 297 */ // skip rt_tgsigqueueinfo (__NR_rt_tgsigqueueinfo)
/* 298 */ // skip perf_event_open (__NR_perf_event_open)
/* 299 */ // skip recvmmsg (__NR_recvmmsg)
/* 300 */ // skip fanotify_init (__NR_fanotify_init)
/* 301 */ // skip fanotify_mark (__NR_fanotify_mark)
/* 302 */ // skip prlimit64 (__NR_prlimit64)
/* 303 */ // skip name_to_handle_at (__NR_name_to_handle_at)
/* 304 */ // skip open_by_handle_at (__NR_open_by_handle_at)
/* 305 */ // skip clock_adjtime (__NR_clock_adjtime)
/* 306 */ // skip syncfs (__NR_syncfs)
/* 307 */ // skip sendmmsg (__NR_sendmmsg)
/* 308 */ // skip setns (__NR_setns)
/* 309 */ // skip getcpu (__NR_getcpu)
/* 310 */ // skip process_vm_readv (__NR_process_vm_readv)
/* 311 */ // skip process_vm_writev (__NR_process_vm_writev)
/* 312 */ // skip kcmp (__NR_kcmp)
/* 313 */ // skip finit_module (__NR_finit_module)
/* 314 */ // skip sched_setattr (__NR_sched_setattr)
/* 315 */ // skip sched_getattr (__NR_sched_getattr)
/* 316 */ TRACE(__NR_renameat2, renameat2);
/* 317 */ // skip seccomp (__NR_seccomp)
/* 318 */ // skip getrandom (__NR_getrandom)
/* 319 */ // skip memfd_create (__NR_memfd_create)
/* 320 */ // skip kexec_file_load (__NR_kexec_file_load)
/* 321 */ // skip bpf (__NR_bpf)
/* 322 */ TRACE(__NR_execveat, execveat);
/* 323 */ // skip userfaultfd (__NR_userfaultfd)
/* 324 */ // skip membarrier (__NR_membarrier)
/* 325 */ // skip mlock2 (__NR_mlock2)
/* 326 */ TRACE(__NR_copy_file_range, copy_file_range);
/* 327 */ TRACE(__NR_preadv2, preadv2);
/* 328 */ TRACE(__NR_pwritev2, pwritev2);
/* 329 */ // skip pkey_mprotect (__NR_pkey_mprotect)
/* 330 */ // skip pkey_alloc (__NR_pkey_alloc)
/* 331 */ // skip pkey_free (__NR_pkey_free)
/* 332 */ TRACE(__NR_statx, statx);
/* 333 */ // skip io_pgetevents (__NR_io_pgetevents)
/* 334 */ // skip rseq (__NR_rseq)
/* 424 */ // skip pidfd_send_signal (__NR_pidfd_send_signal)
/* 425 */ // skip io_uring_setup (__NR_io_uring_setup)
/* 426 */ // skip io_uring_enter (__NR_io_uring_enter)
/* 427 */ // skip io_uring_register (__NR_io_uring_register)
/* 428 */ // skip open_tree (__NR_open_tree)
/* 429 */ // skip move_mount (__NR_move_mount)
/* 430 */ // skip fsopen (__NR_fsopen)
/* 431 */ // skip fsconfig (__NR_fsconfig)
/* 432 */ // skip fsmount (__NR_fsmount)
/* 433 */ // skip fspick (__NR_fspick)
/* 434 */ // skip pidfd_open (__NR_pidfd_open)
/* 435 */ // skip clone3 (__NR_clone3)
