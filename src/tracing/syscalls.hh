#pragma once

#include <cstdint>

#include <syscall.h>

#define SYSCALL_NUMBER orig_rax
#define SYSCALL_RETURN rax
#define SYSCALL_ARG1 rdi
#define SYSCALL_ARG2 rsi
#define SYSCALL_ARG3 rdx
#define SYSCALL_ARG4 r10
#define SYSCALL_ARG5 r8
#define SYSCALL_ARG6 r9

static const uint16_t syscalls[] = {
    /* 0 */ __NR_read,
    /* 1 */ __NR_write,
    /* 2 */ __NR_open,
    /* 3 */ __NR_close,
    /* 9 */ __NR_mmap,
    /* 17 */ __NR_pread64,
    /* 18 */ __NR_pwrite64,
    /* 19 */ __NR_readv,
    /* 20 */ __NR_writev,
    /* 22 */ __NR_pipe,
    /* 32 */ __NR_dup,
    /* 33 */ __NR_dup2,
    /* 40 */ __NR_sendfile,
    /* 72 */ __NR_fcntl,
    /* 76 */ __NR_truncate,
    /* 77 */ __NR_ftruncate,
    /* 78 */ __NR_getdents,
    /* 80 */ __NR_chdir,
    /* 81 */ __NR_fchdir,
    /* 82 */ __NR_rename,
    /* 83 */ __NR_mkdir,
    /* 84 */ __NR_rmdir,
    /* 85 */ __NR_creat,
    /* 86 */ __NR_link,
    /* 87 */ __NR_unlink,
    /* 88 */ __NR_symlink,
    /* 89 */ __NR_readlink,
    /* 133 */ __NR_mknod,
    /* 155 */ __NR_pivot_root,  // ????
    /* 161 */ __NR_chroot,
    /* 217 */ __NR_getdents64,
    /* 257 */ __NR_openat,
    /* 258 */ __NR_mkdirat,
    /* 259 */ __NR_mknodat,
    /* 263 */ __NR_unlinkat,
    /* 264 */ __NR_renameat,
    /* 266 */ __NR_symlinkat,
    /* 267 */ __NR_readlinkat,
    /* 275 */ __NR_splice,
    /* 276 */ __NR_tee,
    /* 278 */ __NR_vmsplice,
    /* 292 */ __NR_dup3,
    /* 293 */ __NR_pipe2,
    /* 295 */ __NR_preadv,
    /* 296 */ __NR_pwritev,
    /* 316 */ __NR_renameat2,
    /* 326 */ __NR_copy_file_range,
    /* 327 */ __NR_preadv2,
    /* 328 */ __NR_pwritev2,
    //--------------- Metadata ops ----------------
    /* 90 */ __NR_chmod,
    /* 91 */ __NR_fchmod,
    /* 92 */ __NR_chown,
    /* 93 */ __NR_fchown,
    /* 94 */ __NR_lchown,
    /* 188 */ __NR_setxattr,
    /* 189 */ __NR_lsetxattr,
    /* 190 */ __NR_fsetxattr,
    // TODO: Do we want to care about read accesses on metadata?
    /* 191 */ __NR_getxattr,
    /* 192 */ __NR_lgetxattr,
    /* 193 */ __NR_fgetxattr,
    /* 194 */ __NR_listxattr,
    /* 195 */ __NR_llistxattr,
    /* 196 */ __NR_flistxattr,
    /* 197 */ __NR_removexattr,
    /* 198 */ __NR_lremovexattr,
    /* 199 */ __NR_fremovexattr,
    /* 260 */ __NR_fchownat,
    /* 268 */ __NR_fchmodat,
    // --------------- Process ops, tracked via ptrace, not seccomp ----------------
    //    /* 56 */ __NR_clone,
    //    /* 57 */ __NR_fork,
    //    /* 58 */ __NR_vfork,
    //    /* 59 */ __NR_execve,
    //    /* 60 */ __NR_exit,
    //    /* 231 */ __NR_exit_group,
    //    /* 322 */ __NR_execveat,
    // --------------- Things we probably should handle but don't -------
    // Asynchronous IO (io_setup, io_destroy, io_getevents, io_submit, io_cancel)
    //    /* 303 */ __NR_name_to_handle_at,
    //    /* 304 */ __NR_open_by_handle_at,
    //
    // -------------- Communication methods we don't handle ----------------
    // XSI shm (shmget, shmat, shmctl, shmdt)
    // Sockets (socket, connect, accept, sendto, recvfrom, sendmsg, recvmsg, shutdown, bind, listen,
    // getsockname, getpeername, socketpair, setsockopt, getsockopt, recvmmsg, sendmmsg) Return
    // codes (wait4, waitid) Signals (kill, rt_sigpending, rt_sigtimeswait, rt_sigqueueinfo,
    // rt_sigsuspent, sigaltstack, tkill, tgkill, pselect, signalfd, epoll_pwait, signalfd4,
    // rt_tgsigqueueinfo) XSI semaphores (semget, semop, semctl, semtimedop) XSI Message Queues
    // (msgget, msgsnd, msgrcv, msgctl) Hard links (link, linkat) + tracking inodes Ptrace (ptrace)
    // FIFOs (mknod with S_IFIFO)
    // System configuration (...lots...)
    // Futexes (futex)
    // POSIX Message Queues (mq_open, mq_unlink, mq_timedsend, mq_timedreceive, mq_notify,
    // mq_getsetattr) Keyrings (add_key, request_key, keyctl) Filesystem watching (inotify_init,
    // inotify_add_watch, inotify_rm_watch, inotify_init1, fanotify_init, fanotify_mark) Eventfd
    // (eventfd, eventfd2) Perf (perf_event_open) Process memory transfers (process_vm_readv,
    // process_vm_writev) Process limits (...lots...)
    // -------------- File access methods we hope we can ignore ----------------
    // stat
    // fstat
    // lstat
    // access
    // flock
    // fsync
    // fdatasync
    // umask
    // utime
    // ustat
    // statfs
    // fstatfs
    // sync
    // acct
    // mount
    // umount2
    // swapon
    // swapoff
    // utimes
    // futimesat
    // newfstatat
    // faccessat
    // sync_file_range
    // utimensat
    // fallocate
    // name_to_handle_at
    // syncfs
    // -------------- File ops we should pay attention to if we ever track file ranges
    // ----------------------
    // __llseek
    // lseek
};
