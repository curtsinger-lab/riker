#pragma once

#include <cstdint>
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

#define SYSCALL_ENTRY(name) { __NR_##name, #name }

map<uint32_t, string> syscalls = {
    SYSCALL_ENTRY(read),
    SYSCALL_ENTRY(write),
    SYSCALL_ENTRY(open),
    SYSCALL_ENTRY(close),
    SYSCALL_ENTRY(stat),
    SYSCALL_ENTRY(fstat),
    SYSCALL_ENTRY(lstat),
    SYSCALL_ENTRY(mmap),
    SYSCALL_ENTRY(pread64),
    SYSCALL_ENTRY(pwrite64),
    SYSCALL_ENTRY(readv),
    SYSCALL_ENTRY(writev),
    SYSCALL_ENTRY(access),
    SYSCALL_ENTRY(pipe),
    SYSCALL_ENTRY(dup),
    SYSCALL_ENTRY(dup2),
    SYSCALL_ENTRY(sendfile),
    SYSCALL_ENTRY(fcntl),
    SYSCALL_ENTRY(truncate),
    SYSCALL_ENTRY(ftruncate),
    SYSCALL_ENTRY(getdents),
    SYSCALL_ENTRY(chdir),
    SYSCALL_ENTRY(fchdir),
    SYSCALL_ENTRY(rename),
    SYSCALL_ENTRY(mkdir),
    SYSCALL_ENTRY(rmdir),
    SYSCALL_ENTRY(creat),
    SYSCALL_ENTRY(link),
    SYSCALL_ENTRY(unlink),
    SYSCALL_ENTRY(symlink),
    SYSCALL_ENTRY(readlink),
    SYSCALL_ENTRY(mknod),
    SYSCALL_ENTRY(pivot_root),
    SYSCALL_ENTRY(chroot),
    SYSCALL_ENTRY(getdents64),
    SYSCALL_ENTRY(openat),
    SYSCALL_ENTRY(mkdirat),
    SYSCALL_ENTRY(mknodat),
    SYSCALL_ENTRY(unlinkat),
    SYSCALL_ENTRY(renameat),
    SYSCALL_ENTRY(symlinkat),
    SYSCALL_ENTRY(readlinkat),
    SYSCALL_ENTRY(splice),
    SYSCALL_ENTRY(tee),
    SYSCALL_ENTRY(vmsplice),
    SYSCALL_ENTRY(dup3),
    SYSCALL_ENTRY(pipe2),
    SYSCALL_ENTRY(preadv),
    SYSCALL_ENTRY(pwritev),
    SYSCALL_ENTRY(renameat2),
    SYSCALL_ENTRY(copy_file_range),
    SYSCALL_ENTRY(preadv2),
    SYSCALL_ENTRY(pwritev2),
    //--------------- Metadata ops ----------------
    SYSCALL_ENTRY(chmod),
    SYSCALL_ENTRY(fchmod),
    SYSCALL_ENTRY(chown),
    SYSCALL_ENTRY(fchown),
    SYSCALL_ENTRY(lchown),
    SYSCALL_ENTRY(setxattr),
    SYSCALL_ENTRY(lsetxattr),
    SYSCALL_ENTRY(fsetxattr),
    SYSCALL_ENTRY(getxattr),
    SYSCALL_ENTRY(lgetxattr),
    SYSCALL_ENTRY(fgetxattr),
    SYSCALL_ENTRY(listxattr),
    SYSCALL_ENTRY(llistxattr),
    SYSCALL_ENTRY(flistxattr),
    SYSCALL_ENTRY(removexattr),
    SYSCALL_ENTRY(lremovexattr),
    SYSCALL_ENTRY(fremovexattr),
    SYSCALL_ENTRY(fchownat),
    SYSCALL_ENTRY(fchmodat),
    SYSCALL_ENTRY(faccessat),
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
