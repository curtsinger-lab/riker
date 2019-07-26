#include <iostream>

#include <cstdlib>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <cstdio>
#include <experimental/filesystem>

#include <memory>

#include <kj/array.h>
#include <kj/vector.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/user.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
#include <fcntl.h>
#include <unistd.h>
#include <linux/seccomp.h>
#include <linux/filter.h>
#include <linux/audit.h>
#include <linux/limits.h>

#include "util.h"
#include "middle.h"
#include "trace.h"
#include "syscalls.h"

// Launch a program via `sh -c`, fully set up with ptrace and seccomp
// to be traced by the current process. launch_traced will return the
// PID of the newly created process, which should be running (or at least
// ready to be waited on) upon return.
static pid_t launch_traced(char const* exec_path, char* const argv[], kj::ArrayPtr<InitialFdEntry const> initial_fds) {
    // In terms of overall structure, this is a bog standard fork/exec spawning function.
    // We always launch the program with /bin/sh, similarly to `system`, which should
    // automatically handle resolving the correct instance of a program and supporting
    // features like redirection.
    //
    // The bulk of the complexity here is setting up tracing. Instead of just attaching
    // ptrace and asking our caller to repeatedly use PTRACE_SYSCALL to step through
    // syscalls, which can be incredibly expensive, we aim to only trigger a stop on
    // a useful subset. To accomplish this, we instally a seccomp-bpf filter that returns
    // SECCOMP_RET_TRACE when we want a stop.
    pid_t child_pid = fork();
    if (child_pid == -1) {
        perror("Failed to fork");
        exit(2);
    } else if (child_pid == 0) {
        // This is the child

        // Set up FDs as requested. We assume that all parent FDs are marked CLOEXEC if
        // necessary and that there are no ordering constraints on duping (e.g. if the
        // child fd for one entry matches the parent fd of another).
        for (size_t fd_index = 0; fd_index < initial_fds.size(); fd_index++) {
            if (dup2(initial_fds[fd_index].parent_fd, initial_fds[fd_index].child_fd) != initial_fds[fd_index].child_fd) {
                perror("Failed to initialize FDs");
                // It is generally better to exit directly within a forked child so that we
                // don't duplicate atexit effects. (This would be strictly required if we used
                // vfork.)
                _exit(2);
            }
        }

        // Allow ourselves to be traced by our parent
        if (ptrace(PTRACE_TRACEME, 0, nullptr, nullptr) != 0) {
            perror("Failed to start tracing");
            _exit(2);
        }

        // Lock down the process so that we are allowed to
        // use seccomp without special permissions
        if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) {
            perror("Failed to allow seccomp");
            _exit(2);
        }

        // Set up the seccomp filter. Since we are handling so
        // many syscalls, we don't want to just use a long list
        // of chained comparisons. Therefore, we first search
        // the syscall number to decide which block of 32 we are in,
        // then look up the result in a bitset.
        uint32_t bitset[11] = {0};
        for (size_t i = 0; i < ARRAY_COUNT(syscalls); i++) {
            bitset[syscalls[i] >> 5] |= 1 << (syscalls[i] & 0x1F);
        }

        // The actual binary BPF code
        struct sock_filter bpf_filter[] = {
            // Ensure that we are using the x86_64 syscall interface.
            // TODO: support other architectures
            BPF_STMT(BPF_LD+BPF_W+BPF_ABS, offsetof(struct seccomp_data, arch)),
            BPF_JUMP(BPF_JMP+BPF_JEQ+BPF_K, AUDIT_ARCH_X86_64, 1, 0), // Check that this is x86_64
            BPF_STMT(BPF_RET+BPF_K, SECCOMP_RET_KILL),
            BPF_STMT(BPF_LD+BPF_W+BPF_ABS, offsetof(struct seccomp_data, nr)),
            BPF_JUMP(BPF_JMP+BPF_JSET+BPF_K, __X32_SYSCALL_BIT, 0, 1), // And we are actually running a 64-bit program
            BPF_STMT(BPF_RET+BPF_K, SECCOMP_RET_KILL),

            // We stash the low bits of the syscall number for when we use it to look up
            // in a 32-bit set
            BPF_STMT(BPF_LD+BPF_W+BPF_ABS, offsetof(struct seccomp_data, nr)),
            BPF_STMT(BPF_ALU+BPF_AND+BPF_K, 0x1F),
            BPF_STMT(BPF_MISC+BPF_TAX, 0),

            // Look for the correct 32-bit block.
            BPF_STMT(BPF_LD+BPF_W+BPF_ABS, offsetof(struct seccomp_data, nr)),

#define BITSET_BLOCK(index, total) \
            /* For each block, test if we are in the block */                  \
            BPF_JUMP(BPF_JMP+BPF_JGE+BPF_K, 32 * (index + 1), 2, 0),      \
            /* then load the appropriate bitset into the register */           \
            BPF_STMT(BPF_LD+BPF_W+BPF_IMM, bitset[index]),               \
            /* then jump to the testing code, past the catch-all instruction */ \
            BPF_JUMP(BPF_JMP+BPF_JA, (total - 1 - index) * 3 + 1, 0, 0)

            BITSET_BLOCK(0,  11),
            BITSET_BLOCK(1,  11),
            BITSET_BLOCK(2,  11),
            BITSET_BLOCK(3,  11),
            BITSET_BLOCK(4,  11),
            BITSET_BLOCK(5,  11),
            BITSET_BLOCK(6,  11),
            BITSET_BLOCK(7,  11),
            BITSET_BLOCK(8,  11),
            BITSET_BLOCK(9,  11),
            BITSET_BLOCK(10, 11),
            // The catch-all instruction will say not to trace any super-high syscall number
            // that we don't recognize
            BPF_STMT(BPF_RET+BPF_K, SECCOMP_RET_ALLOW),

            // Shift our bitset and extract the low bit to test the correct bit
            BPF_STMT(BPF_ALU+BPF_RSH+BPF_X, 0),
            BPF_JUMP(BPF_JMP+BPF_JSET+BPF_K, 1, 1 /* trace */, 0 /* allow */),
            BPF_STMT(BPF_RET+BPF_K, SECCOMP_RET_ALLOW),
            // TODO: Do more filtering here: for example, we don't care about private,
            // anonymous memory mappings or likely about interactions with stdin/stdout/stderr.
            BPF_STMT(BPF_RET+BPF_K, SECCOMP_RET_TRACE),
        };

        struct sock_fprog bpf_program;
        bpf_program.filter = bpf_filter;
        bpf_program.len = (unsigned short) ARRAY_COUNT(bpf_filter);

        // Actually enable the filter
        if (prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &bpf_program) != 0) {
            perror("Error enabling seccomp");
            _exit(2);
        }

        // We need to stop here to give our parent a consistent time to add all the
        // options and correctly configure ptrace. Otherwise it will get a very confusing
        // response upon exec:
        // - Because PTRACE_O_TRACEEXEC has not been set (or worse, is racing with the call
        //   to exec), the parent may receive a SIGTRAP-stop on the exec.
        // - Because our seccomp program traps execve, the it will attempt to send our parent
        //   a seccomp stop when the parent is not configured to receive one.
        // Therefore, to ensure reliable behavior, we wait here, let the parent configure ptrace,
        // then continue to exec, which will raise two stops that the parent is (now) expecting
        // to handle.
        raise(SIGSTOP);

        // TODO: explicitly handle the environment
        execv(exec_path, argv);

        // If we reach here, we failed to exec
        perror("Executing shell");
        _exit(2);
    }

    // In the parent. Wait for the child to reach its exec so that we
    // have a consistent point to play in. Here we haven't yet set
    // PTRACE_O_TRACEEXEC, so we will receive the legacy behavior of
    // a SIGTRAP.
    int wstatus;
    waitpid(child_pid, &wstatus, 0); // Should correspond to raise(SIGSTOP)
    if (!WIFSTOPPED(wstatus) || WSTOPSIG(wstatus) != SIGSTOP) {
        fprintf(stderr, "Unexpected stop from child, expected SIGSTOP: %x\n", wstatus);
        exit(2);
    }

    // Set up options to handle everything reliably. We do this before continuing
    // so that the actual running program has everything properly configured.
    if (ptrace(PTRACE_SETOPTIONS, child_pid, nullptr,
        PTRACE_O_TRACEFORK | PTRACE_O_TRACECLONE | PTRACE_O_TRACEVFORK | // Follow forks
        PTRACE_O_TRACEEXEC | // Handle execs more reliably
        PTRACE_O_TRACESYSGOOD | // When stepping through syscalls, be clear
        PTRACE_O_TRACESECCOMP // Actually receive the syscall stops we requested
    ) != 0) {
        perror("Failed to set options");
        exit(2);
    }

    // Let the child restart to reach its exec.
    ptrace(PTRACE_CONT, child_pid, nullptr, 0);

    // Let the child finish its exec.
    waitpid(child_pid, &wstatus, 0); // Should correspond to SECCOMP_RET_TRACE for execve
    if (!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
        fprintf(stderr, "Unexpected stop from child, expected SECCOMP: %x\n", wstatus);
        exit(2);
    }
    ptrace(PTRACE_CONT, child_pid, nullptr, 0);
    waitpid(child_pid, &wstatus, 0); // Should correspond to inside the exec
    if (!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        fprintf(stderr, "Unexpected stop from child, expected EXEC: %x\n", wstatus);
        exit(2);
    }
    ptrace(PTRACE_CONT, child_pid, nullptr, 0);
    return child_pid;
}

enum stop_type {
    STOP_SYSCALL,
    STOP_FORK,
    STOP_EXEC,
    STOP_CLONE,
    STOP_EXIT, // Do not resume when this is returned
};

static Blob read_tracee_string(pid_t process, uintptr_t tracee_pointer) {
    kj::Vector<kj::byte> output;

    // Loop to fetch words at a time until we find a null byte
    while (true) {
        // To properly check for errors when doing PEEKDATA, we need to clear then check errno,
        // since PEEKDATA could validly return -1.
        errno = 0;
        long data = ptrace(PTRACE_PEEKDATA, process, tracee_pointer + output.size(), nullptr);
        if (errno != 0) {
            perror("Failed to read tracee string");
            exit(2);
        }

        // Copy in the data
        for (size_t i = 0; i < sizeof(long); i++) {
            if (((kj::byte*) &data)[i] == '\0') {
                return output.releaseAsArray();
            } else {
                output.add(((kj::byte*) &data)[i]);
            }
        }
    }
}

void start_command(Command* cmd, kj::ArrayPtr<InitialFdEntry const> initial_fds) {
    std::string exec_path = std::string(cmd->cmd.asPtr().asChars().begin(), cmd->cmd.asPtr().size());
    std::vector<char*> exec_argv;
    for (auto arg = cmd->args.begin(); arg != cmd->args.end(); ++arg) {
       char* c_arg = new char[arg->asPtr().size() + 1];
       memcpy(c_arg, arg->asPtr().begin(), arg->asPtr().size());
       c_arg[arg->asPtr().size()] = '\0';
       exec_argv.push_back(c_arg);
    }
    exec_argv.push_back(nullptr);
    pid_t pid = launch_traced(exec_path.c_str(), exec_argv.data(), initial_fds);
    for (auto arg : exec_argv) {
        delete arg;
    }

    Process* proc = new Process(pid, kj::heapArray(cmd->state->starting_dir.asPtr()), cmd);
    cmd->state->processes.insert(std::pair<pid_t, Process*>(pid, proc));
}

void trace_step(trace_state* state, pid_t child, int wait_status) {
    enum stop_type stop_ty;

    if (WIFSTOPPED(wait_status)) {
        switch (wait_status >> 8) {
        case (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8)):
            stop_ty = STOP_SYSCALL;
            break;
        case (SIGTRAP | (PTRACE_EVENT_FORK << 8)):
        case (SIGTRAP | (PTRACE_EVENT_VFORK << 8)):
            stop_ty = STOP_FORK;
            break;
        case (SIGTRAP | (PTRACE_EVENT_EXEC << 8)):
            stop_ty = STOP_EXEC;
            break;
        case (SIGTRAP | (PTRACE_EVENT_CLONE<<8)):
            stop_ty = STOP_CLONE;
            break;
        default:
            // We don't bother handling errors here, because any failure
            // just means that the child is somehow broken, and we shouldn't
            // continue processing it anyway.
            ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
            return;
        }
    } else if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
        stop_ty = STOP_EXIT;
    } else {
        return;
    }

    Process* proc = state->processes.find(child)->second;

    switch (stop_ty) {
    case STOP_FORK: {
        // GETEVENTMSG returns a unsigned long *always*, even though the value is
        // always a pid_t.
        unsigned long ul_new_child;
        if (ptrace(PTRACE_GETEVENTMSG, child, nullptr, &ul_new_child) != 0) {
            perror("Unable to fetch new child from fork");
            ptrace(PTRACE_CONT, child, nullptr, 0);
            return;
        }
        pid_t new_child = (pid_t) ul_new_child;
        ptrace(PTRACE_CONT, child, nullptr, 0);
        state->add_fork(proc, new_child);
        break;
    }
    case STOP_EXEC: {
        struct user_regs_struct registers;
        if (ptrace(PTRACE_GETREGS, child, nullptr, &registers) != 0) {
            perror("Failed to get registers");
            ptrace(PTRACE_CONT, child, nullptr, 0);
            return;
        }

        // Load the path to our executable
        char proc_path_buffer[24]; // 24 is long enough for any integer PID
        sprintf(proc_path_buffer, "/proc/%d/exe", child);

        // In practice, many paths are longer than PATH_MAX, so make sure to handle
        // an arbitrary length response from readlink
        kj::Vector<kj::byte> exe_path(PATH_MAX);
        while (true) {
            exe_path.resize(exe_path.capacity());
            size_t bytes_written = readlink(proc_path_buffer, (char*) exe_path.begin(), exe_path.size());
            if (bytes_written < exe_path.size()) {
                exe_path.truncate(bytes_written);
                break;
            } else {
                exe_path.reserve(exe_path.capacity() + 1);
            }
        }

        state->add_exec(proc, exe_path.releaseAsArray());

        int child_argc = ptrace(PTRACE_PEEKDATA, child, registers.rsp, nullptr);
        for (int i = 0; i < child_argc; i++) {
            uintptr_t arg_ptr = ptrace(PTRACE_PEEKDATA, child, registers.rsp + (1 + i) * sizeof(long), nullptr);
            state->add_exec_argument(proc, read_tracee_string(child, arg_ptr), i);
        }

        ptrace(PTRACE_CONT, child, nullptr, 0);
        break;
    }
    case STOP_CLONE: {
        unsigned long ul_new_thread;
        if (ptrace(PTRACE_GETEVENTMSG, child, nullptr, &ul_new_thread) != 0) {
            perror("Unable to fetch new child from fork");
            ptrace(PTRACE_CONT, child, nullptr, 0);
            return;
        }
        pid_t new_thread = (pid_t) ul_new_thread;
        ptrace(PTRACE_CONT, child, nullptr, 0);
        // TODO handling of flags -> CLONE_FILES
        state->processes[new_thread] = proc;
        break;
    }
    case STOP_EXIT: {
        state->add_exit(proc);
        break;
    }
    case STOP_SYSCALL: {
        struct user_regs_struct registers;
        if (ptrace(PTRACE_GETREGS, child, nullptr, &registers) != 0) {
            perror("Failed to get registers");
            ptrace(PTRACE_CONT, child, nullptr, 0);
            return;
        }

        // Certain syscalls have cases that we just don't want to handle. Detect those
        // first and skip all the work we can. TODO: do this filtering in seccomp
        switch (registers.SYSCALL_NUMBER) {
        case /* 9 */ __NR_mmap:
            if ((registers.SYSCALL_ARG4 & MAP_ANONYMOUS) != 0) {
                ptrace(PTRACE_CONT, child, nullptr, 0);
                return;
            }
            break;
        case /* 72 */ __NR_fcntl:
            switch (registers.SYSCALL_ARG2) {
            case F_DUPFD:
            case F_DUPFD_CLOEXEC:
            case F_SETFD:
                break;
            default:
                ptrace(PTRACE_CONT, child, nullptr, 0);
                return;
            }
            break;
        }

        // We need to extract any relevant arguments like paths or file names
        // before we tell the process to continue. However, we want to do minimal
        // work so that we can exploit the most parallelism, so we just read the
        // path(s) and nothing else.
        struct file_reference main_file = {
            .fd = AT_FDCWD,
            .path = nullptr,
            .follow_links = true,
        };
        struct file_reference extra_file = {
            .fd = AT_FDCWD,
            .path = nullptr,
            .follow_links = true,
        };
        switch (registers.SYSCALL_NUMBER) {
        // Many operations dealing with files take a single path as their first argument
        case /* 2 */ __NR_open:
        case /* 59 */ __NR_execve:
        case /* 76 */ __NR_truncate:
        case /* 80 */ __NR_chdir:
        case /* 83 */ __NR_mkdir:
        case /* 84 */ __NR_rmdir:
        case /* 85 */ __NR_creat:
        case /* 87 */ __NR_unlink:
        case /* 89 */ __NR_readlink:
        case /* 133 */ __NR_mknod:
        case /* 161 */ __NR_chroot:
        case /* 90 */ __NR_chmod:
        case /* 92 */ __NR_chown:
        case /* 94 */ __NR_lchown:
        case /* 188 */ __NR_setxattr:
        case /* 189 */ __NR_lsetxattr:
        case /* 191 */ __NR_getxattr:
        case /* 192 */ __NR_lgetxattr:
        case /* 194 */ __NR_listxattr:
        case /* 195 */ __NR_llistxattr:
        case /* 197 */ __NR_removexattr:
        case /* 198 */ __NR_lremovexattr:
            main_file.path = read_tracee_string(child, registers.SYSCALL_ARG1);
            break;
        // The *at class of syscalls take a directory fd in their first argument, so the
        // path is given in the second argument
        case /* 257 */ __NR_openat:
        case /* 258 */ __NR_mkdirat:
        case /* 259 */ __NR_mknodat:
        case /* 263 */ __NR_unlinkat:
        case /* 260 */ __NR_fchownat:
        case /* 267 */ __NR_readlinkat:
        case /* 268 */ __NR_fchmodat:
        case /* 322 */ __NR_execveat:
            main_file.fd = registers.SYSCALL_ARG1;
            main_file.path = read_tracee_string(child, registers.SYSCALL_ARG2);
            break;
        // Some more complicated data transfer operations require two paths for a source
        // and a destination file, broadly interpreted
        case /* 82 */ __NR_rename:
        case /* 155 */ __NR_pivot_root:
            main_file.path = read_tracee_string(child, registers.SYSCALL_ARG1);
            extra_file.path = read_tracee_string(child, registers.SYSCALL_ARG2);
            break;
        // The multi-file operations also have *at forms, with directory fds before both
        // of the paths
        case /* 264 */ __NR_renameat:
        case /* 316 */ __NR_renameat2:
            main_file.fd = registers.SYSCALL_ARG1;
            main_file.path = read_tracee_string(child, registers.SYSCALL_ARG2);
            extra_file.fd = registers.SYSCALL_ARG3;
            extra_file.path = read_tracee_string(child, registers.SYSCALL_ARG4);
            break;
        // Symlinks are a bit odd because they don't actually read what is being referenced
        // and in fact the reference doesn't have to exist. We just ignore that argument.
        case /* 88 */ __NR_symlink:
            main_file.path = read_tracee_string(child, registers.SYSCALL_ARG2);
            break;
        case /* 266 */ __NR_symlinkat:
            main_file.fd = registers.SYSCALL_ARG2;
            main_file.path = read_tracee_string(child, registers.SYSCALL_ARG3);
            break;
        // The following syscalls deal only in already-open file descriptors,
        // so record those.
        case /* 0 */ __NR_read:
        case /* 1 */ __NR_write:
        case /* 17 */ __NR_pread64:
        case /* 18 */ __NR_pwrite64:
        case /* 19 */ __NR_readv:
        case /* 20 */ __NR_writev:
        case /* 72 */ __NR_fcntl:
        case /* 77 */ __NR_ftruncate:
        case /* 78 */ __NR_getdents:
        case /* 217 */ __NR_getdents64:
        case /* 278 */ __NR_vmsplice:
        case /* 295 */ __NR_preadv:
        case /* 296 */ __NR_pwritev:
        case /* 327 */ __NR_preadv2:
        case /* 328 */ __NR_pwritev2:
        case /* 91 */ __NR_fchmod:
        case /* 93 */ __NR_fchown:
        case /* 190 */ __NR_fsetxattr:
        case /* 193 */ __NR_fgetxattr:
        case /* 196 */ __NR_flistxattr:
        case /* 199 */ __NR_fremovexattr:
            main_file.fd = registers.SYSCALL_ARG1;
            break;
        case /* 9 */ __NR_mmap:
            main_file.fd = registers.SYSCALL_ARG5;
            break;
        case /* 40 */ __NR_sendfile:
            main_file.fd = registers.SYSCALL_ARG2;
            extra_file.fd = registers.SYSCALL_ARG1;
            break;
        case /* 275 */ __NR_splice:
        case /* 326 */ __NR_copy_file_range:
            main_file.fd = registers.SYSCALL_ARG1;
            extra_file.fd = registers.SYSCALL_ARG3;
            break;
        case /* 276 */ __NR_tee:
            main_file.fd = registers.SYSCALL_ARG1;
            extra_file.fd = registers.SYSCALL_ARG2;
            break;
        }

        // We need to know the return values of a few syscalls, specifically those that
        // allocate new fds. Therefore, use PTRACE_SYSCALL to step past just the syscall
        // and reinspect the process.
        switch (registers.SYSCALL_NUMBER) {
        case /* 2 */ __NR_open:
        case /* 22 */ __NR_pipe:
        case /* 32 */ __NR_dup:
        case /* 72 */ __NR_fcntl:
        case /* 85 */ __NR_creat:
        case /* 257 */ __NR_openat:
        case /* 293 */ __NR_pipe2:
            // We've already hit the syscall-enter, so wait for the syscall-exit.
            ptrace(PTRACE_SYSCALL, child, nullptr, 0);
            waitpid(child, nullptr, 0); // FIXME: handle errors
            // PEEKUSER should hopefully be more efficient than GETREGS here because
            // we only care about a single register
            registers.SYSCALL_RETURN = ptrace(PTRACE_PEEKUSER, child, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
            if ((long long)registers.SYSCALL_RETURN < 0) { // The call errored, so skip it
                ptrace(PTRACE_CONT, child, nullptr, 0);
                return;
            }
            break;
        }

        // The pipe and pipe2 syscalls return their new file descriptors through a user array,
        // so read that data.
        int pipe_fds[2];
        switch (registers.SYSCALL_NUMBER) {
        case /* 22 */ __NR_pipe:
        case /* 293 */ __NR_pipe2:
            pipe_fds[0] = ptrace(PTRACE_PEEKDATA, child, registers.SYSCALL_ARG1, 0);
            pipe_fds[1] = ptrace(PTRACE_PEEKDATA, child, registers.SYSCALL_ARG1 + sizeof(int), 0);
            break;
        }

        // Relaunch the child. After this point, we can no longer access the child's memory,
        // but we are allowed to do slightly more expensive things.
        // FIXME: We previously continued here, but this broke instances where we
        // needed a fingerprint. Contnuing at the end of the loop doesn't allow us to run
        // in parallel, so we want to continue as soon as possible. Therefore, once we have
        // any fingerprints we need, call continue immediately.

        // Handle fake-relative syscalls by ignoring the fd when the path is absolute
        if (main_file.path != nullptr && main_file.path[0] == '/') {
            main_file.fd = AT_FDCWD;
        }
        if (extra_file.path != nullptr && extra_file.path[0] == '/') {
            extra_file.fd = AT_FDCWD;
        }

        // Any syscall that references a path can implicitly dereference symlinks while
        // traversing the path. This is true even of the l-prefixed syscalls designed to not
        // dereference symlinks, since that change only applies to the final symlink;
        // directories along the way are still dereferenced.
        //
        // Instead of expensively implementing user-space path resolution and depending on
        // every symlink individually, we just assume that symlinks won't change over the
        // course of the build and only keep track of the distinction for the final file.
        switch (registers.SYSCALL_NUMBER) {
        case /* 83 */ __NR_mkdir:
        case /* 84 */ __NR_rmdir:
        case /* 87 */ __NR_unlink:
        case /* 88 */ __NR_symlink:
        case /* 89 */ __NR_readlink:
        case /* 94 */ __NR_lchown:
        case /* 133 */ __NR_mknod:
        case /* 189 */ __NR_lsetxattr:
        case /* 192 */ __NR_lgetxattr:
        case /* 195 */ __NR_llistxattr:
        case /* 198 */ __NR_lremovexattr:
        case /* 258 */ __NR_mkdirat:
        case /* 259 */ __NR_mknodat:
        case /* 263 */ __NR_unlinkat:
        case /* 266 */ __NR_symlinkat:
        case /* 267 */ __NR_readlinkat:
            main_file.follow_links = false;
            break;
        }

        // If this syscall only deals with something in /proc/self, then assume that it is messing with
        // process-internal state and we can safely ignore it.
        // FIXME: There are paths through /proc/self that lead out of procfs, such as
        // referencing a something in a directory linked by /proc/self (i.e. /proc/self/fd/10/foo/bar)
        if (!main_file.follow_links &&
            main_file.fd == AT_FDCWD &&
            main_file.path.size() >= 11 &&
            main_file.path.slice(0, 11).asChars().asConst() == kj::arrayPtr("/proc/self/", 11)) {
            ptrace(PTRACE_CONT, child, nullptr, 0);
            return;
        }

        // This is the giant switch statement where we handle what every syscall means. Note
        // that we don't handle execve and execveat, since the actual processing there is done
        // on STOP_EXEC (i.e. PTRACE_EVENT_EXEC).
        switch (registers.SYSCALL_NUMBER) {
        ////// Fiddling with file descriptors //////
        case /* 3 */ __NR_close:
            state->add_close(proc, registers.SYSCALL_ARG1);
            break;
        case /* 2 */ __NR_open:
        case /* 85 */ __NR_creat:
        case /* 257 */ __NR_openat: {
            int flags;
            mode_t mode;
            switch (registers.SYSCALL_NUMBER) {
            case __NR_open:
                flags = registers.SYSCALL_ARG2;
                mode = registers.SYSCALL_ARG3;
                break;
            case __NR_creat:
                flags = O_CREAT | O_WRONLY | O_TRUNC;
                mode = registers.SYSCALL_ARG2;
                break;
            case __NR_openat:
                flags = registers.SYSCALL_ARG3;
                mode = registers.SYSCALL_ARG4;
                break;
            }

            int access_mode = flags & (O_RDONLY | O_WRONLY | O_RDWR);
            if ((flags & O_EXCL) != 0 || (flags & O_NOFOLLOW) != 0) {
                main_file.follow_links = false;
            }
            bool rewrite = ((flags & O_EXCL) != 0 || (flags & O_TRUNC) != 0);
            bool cloexec = (flags & O_CLOEXEC) != 0;
            state->add_open(proc, registers.SYSCALL_RETURN, main_file, access_mode, rewrite, cloexec, mode);
            break;
        }
        case /* 22 */ __NR_pipe:
            state->add_pipe(proc, pipe_fds, false);
            break;
        case /* 293 */ __NR_pipe2:
            state->add_pipe(proc, pipe_fds, (registers.SYSCALL_ARG2 & O_CLOEXEC) != 0);
            break;
        case /* 32 */ __NR_dup:
            state->add_dup(proc, registers.SYSCALL_ARG1, registers.SYSCALL_RETURN, false);
            break;
        case /* 33 */ __NR_dup2:
            state->add_dup(proc, registers.SYSCALL_ARG1, registers.SYSCALL_ARG2, false);
            break;
        case /* 292 */ __NR_dup3:
            state->add_dup(proc, registers.SYSCALL_ARG1, registers.SYSCALL_ARG2, (registers.SYSCALL_ARG3 & O_CLOEXEC) != 0);
            break;
        case /* 72 */ __NR_fcntl:
            switch (registers.SYSCALL_ARG2) {
            case F_DUPFD:
                state->add_dup(proc, registers.SYSCALL_ARG1, registers.SYSCALL_RETURN, false);
                break;
            case F_DUPFD_CLOEXEC:
                state->add_dup(proc, registers.SYSCALL_ARG1, registers.SYSCALL_RETURN, true);
                break;
            case F_SETFD:
                state->add_set_cloexec(proc, registers.SYSCALL_ARG1, (registers.SYSCALL_ARG3 & FD_CLOEXEC) != 0);
                break;
            }
            break;
        ////// Changing process state //////
        case /* 80 */ __NR_chdir:
        case /* 81 */ __NR_fchdir:
            state->add_change_cwd(proc, main_file);
            break;
        case /* 161 */ __NR_chroot:
            state->add_change_root(proc, main_file);
            break;
        ////// Complex operations /////
        case /* 9 */ __NR_mmap:
            state->add_mmap(proc, main_file.fd);
            break;
        case /* 40 */ __NR_sendfile:
        case /* 275 */ __NR_splice:
        case /* 276 */ __NR_tee:
        case /* 326 */ __NR_copy_file_range:
            state->add_dependency(proc, main_file, DEP_READ);
            state->add_dependency(proc, extra_file, DEP_MODIFY);
            break;
        case /* 76 */ __NR_truncate:
        case /* 77 */ __NR_ftruncate:
            if (registers.SYSCALL_ARG2 == 0) {
                state->add_dependency(proc, main_file, DEP_REMOVE);
                state->add_dependency(proc, main_file, DEP_CREATE);
            } else {
                state->add_dependency(proc, main_file, DEP_MODIFY);
            }
            break;
        case /* 82 */ __NR_rename:
        case /* 264 */ __NR_renameat:
        case /* 316 */ __NR_renameat2: // TODO: Handle the flags
            state->add_dependency(proc, main_file, DEP_READ);
            state->add_dependency(proc, main_file, DEP_REMOVE);
            state->add_dependency(proc, extra_file, DEP_REMOVE);
            state->add_dependency(proc, extra_file, DEP_CREATE);
            state->add_dependency(proc, extra_file, DEP_MODIFY);
            break;
        ////// Simple reads and writes //////
        case /* 0 */ __NR_read:
        case /* 17 */ __NR_pread64:
        case /* 19 */ __NR_readv:
        case /* 59 */ __NR_execve:
        case /* 78 */ __NR_getdents:
        case /* 89 */ __NR_readlink:
        case /* 217 */ __NR_getdents64:
        case /* 295 */ __NR_preadv:
        case /* 327 */ __NR_preadv2:
        case /* 191 */ __NR_getxattr:
        case /* 192 */ __NR_lgetxattr:
        case /* 193 */ __NR_fgetxattr:
        case /* 194 */ __NR_listxattr:
        case /* 195 */ __NR_llistxattr:
        case /* 196 */ __NR_flistxattr:
        case /* 267 */ __NR_readlinkat:
        case /* 322 */ __NR_execveat:
            state->add_dependency(proc, main_file, DEP_READ);
            break;
        case /* 1 */ __NR_write:
        case /* 18 */ __NR_pwrite64:
        case /* 20 */ __NR_writev:
        case /* 278 */ __NR_vmsplice:
        case /* 296 */ __NR_pwritev:
        case /* 328 */ __NR_pwritev2:
        case /* 90 */ __NR_chmod:
        case /* 91 */ __NR_fchmod:
        case /* 92 */ __NR_chown:
        case /* 93 */ __NR_fchown:
        case /* 94 */ __NR_lchown:
        case /* 188 */ __NR_setxattr:
        case /* 189 */ __NR_lsetxattr:
        case /* 190 */ __NR_fsetxattr:
        case /* 197 */ __NR_removexattr:
        case /* 198 */ __NR_lremovexattr:
        case /* 199 */ __NR_fremovexattr:
        case /* 260 */ __NR_fchownat:
        case /* 268 */ __NR_fchmodat:
            state->add_dependency(proc, main_file, DEP_MODIFY);
            break;
        case /* 83 */ __NR_mkdir:
        case /* 88 */ __NR_symlink:
        case /* 133 */ __NR_mknod:
        case /* 258 */ __NR_mkdirat:
        case /* 259 */ __NR_mknodat:
        case /* 266 */ __NR_symlinkat:
            state->add_dependency(proc, main_file, DEP_CREATE);
            break;
        case /* 84 */ __NR_rmdir:
        case /* 87 */ __NR_unlink:
        case /* 263 */ __NR_unlinkat:
            state->add_dependency(proc, main_file, DEP_REMOVE);
            break;
        default:
            fprintf(stderr, "[%d] UNHANDLED SYSCALL: %d\n", child, (int)registers.SYSCALL_NUMBER);
            break;
        }

        // FIXME: See FIXME above about why this ought to be earlier in the loop
        ptrace(PTRACE_CONT, child, nullptr, 0);
        break;
    }
    }
}
