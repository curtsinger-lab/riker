#include "Tracer.hh"

#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <list>
#include <memory>
#include <string>
#include <vector>

#include <fcntl.h>
#include <limits.h>
#include <linux/audit.h>
#include <linux/bpf_common.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <syscall.h>
#include <unistd.h>

#include "core/BuildGraph.hh"
#include "core/Command.hh"
#include "core/File.hh"
#include "tracing/syscalls.hh"
#include "ui/log.hh"

using std::list;
using std::make_shared;
using std::shared_ptr;
using std::string;

#define ARRAY_COUNT(array) (sizeof(array) / sizeof(array[0]))

static string get_executable(pid_t pid) {
  char path_buffer[24];  // 24 is long enough for any integer PID
  sprintf(path_buffer, "/proc/%d/exe", pid);

  unique_ptr<char[]> buffer(nullptr);
  ssize_t capacity = 0;
  ssize_t bytes_read = 0;

  do {
    capacity += PATH_MAX;
    buffer = unique_ptr<char[]>(new char[capacity]);
    bytes_read = readlink(path_buffer, buffer.get(), capacity);
  } while (bytes_read == capacity);

  return string(buffer.get(), bytes_read);
}

static string read_tracee_string(pid_t process, uintptr_t tracee_pointer) {
  string output;

  // Loop to fetch words at a time until we find a null byte
  while (true) {
    // To properly check for errors when doing PEEKDATA, we need to clear then check errno,
    // since PEEKDATA could validly return -1.
    errno = 0;
    long data = ptrace(PTRACE_PEEKDATA, process, tracee_pointer + output.size(), nullptr);
    FAIL_IF(errno != 0) << "Failed to read string from traced process: " << ERR;

    // Copy in the data
    for (size_t i = 0; i < sizeof(long); i++) {
      char c = ((char*)&data)[i];
      if (c == '\0')
        return output;
      else
        output.push_back(c);
    }
  }
}

static uintptr_t read_tracee_data(pid_t process, uintptr_t tracee_pointer) {
  // Clear errno so we can detect errors
  errno = 0;
  uintptr_t result = ptrace(PTRACE_PEEKDATA, process, tracee_pointer, nullptr);
  FAIL_IF(errno != 0) << "Failed to read data from traced process: " << ERR;
  return result;
}

// Launch a program via `sh -c`, fully set up with ptrace and seccomp
// to be traced by the current process. launch_traced will return the
// PID of the newly created process, which should be running (or at least
// ready to be waited on) upon return.
static pid_t launch_traced(char const* exec_path, char* const argv[],
                           vector<InitialFdEntry> initial_fds) {
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
  FAIL_IF(child_pid == -1) << "Failed to fork: " << ERR;

  if (child_pid == 0) {
    // This is the child

    // Set up FDs as requested. We assume that all parent FDs are marked CLOEXEC if
    // necessary and that there are no ordering constraints on duping (e.g. if the
    // child fd for one entry matches the parent fd of another).
    for (size_t fd_index = 0; fd_index < initial_fds.size(); fd_index++) {
      int rc = dup2(initial_fds[fd_index].parent_fd, initial_fds[fd_index].child_fd);
      FAIL_IF(rc != initial_fds[fd_index].child_fd) << "Failed to initialize fds: " << ERR;
    }

    // Allow ourselves to be traced by our parent
    FAIL_IF(ptrace(PTRACE_TRACEME, 0, nullptr, nullptr) != 0) << "Failed to start tracing: " << ERR;

    // Lock down the process so that we are allowed to
    // use seccomp without special permissions
    FAIL_IF(prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) << "Failed to allow seccomp: " << ERR;

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
        BPF_STMT(BPF_LD + BPF_W + BPF_ABS, offsetof(struct seccomp_data, arch)),
        BPF_JUMP(BPF_JMP + BPF_JEQ + BPF_K, AUDIT_ARCH_X86_64, 1, 0),  // Check that this is x86_64
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_KILL),
        BPF_STMT(BPF_LD + BPF_W + BPF_ABS, offsetof(struct seccomp_data, nr)),
        BPF_JUMP(BPF_JMP + BPF_JSET + BPF_K, __X32_SYSCALL_BIT, 0,
                 1),  // And we are actually running a 64-bit program
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_KILL),

        // We stash the low bits of the syscall number for when we use it to look up
        // in a 32-bit set
        BPF_STMT(BPF_LD + BPF_W + BPF_ABS, offsetof(struct seccomp_data, nr)),
        BPF_STMT(BPF_ALU + BPF_AND + BPF_K, 0x1F),
        BPF_STMT(BPF_MISC + BPF_TAX, 0),

        // Look for the correct 32-bit block.
        BPF_STMT(BPF_LD + BPF_W + BPF_ABS, offsetof(struct seccomp_data, nr)),

#define BITSET_BLOCK(index, total)                                                                 \
  /* For each block, test if we are in the block */                                                \
  BPF_JUMP(BPF_JMP + BPF_JGE + BPF_K, 32 * (index + 1), 2,                                         \
           0), /* then load the appropriate bitset into the register */                            \
      BPF_STMT(BPF_LD + BPF_W + BPF_IMM,                                                           \
               bitset[index]), /* then jump to the testing code, past the catch-all instruction */ \
      BPF_JUMP(BPF_JMP + BPF_JA, (total - 1 - index) * 3 + 1, 0, 0)

        BITSET_BLOCK(0, 11),
        BITSET_BLOCK(1, 11),
        BITSET_BLOCK(2, 11),
        BITSET_BLOCK(3, 11),
        BITSET_BLOCK(4, 11),
        BITSET_BLOCK(5, 11),
        BITSET_BLOCK(6, 11),
        BITSET_BLOCK(7, 11),
        BITSET_BLOCK(8, 11),
        BITSET_BLOCK(9, 11),
        BITSET_BLOCK(10, 11),
        // The catch-all instruction will say not to trace any super-high syscall number
        // that we don't recognize
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_ALLOW),

        // Shift our bitset and extract the low bit to test the correct bit
        BPF_STMT(BPF_ALU + BPF_RSH + BPF_X, 0),
        BPF_JUMP(BPF_JMP + BPF_JSET + BPF_K, 1, 1 /* trace */, 0 /* allow */),
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_ALLOW),
        // TODO: Do more filtering here: for example, we don't care about private,
        // anonymous memory mappings or likely about interactions with stdin/stdout/stderr.
        BPF_STMT(BPF_RET + BPF_K, SECCOMP_RET_TRACE),
    };

    struct sock_fprog bpf_program;
    bpf_program.filter = bpf_filter;
    bpf_program.len = (unsigned short)ARRAY_COUNT(bpf_filter);

    // Actually enable the filter
    FAIL_IF(prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &bpf_program) != 0)
        << "Error enabling seccomp: " << ERR;

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

    // This is unreachable, unless execv fails
    FAIL << "Failed to start traced program: " << ERR;
  }

  // In the parent. Wait for the child to reach its exec so that we
  // have a consistent point to play in. Here we haven't yet set
  // PTRACE_O_TRACEEXEC, so we will receive the legacy behavior of
  // a SIGTRAP.
  int wstatus;
  waitpid(child_pid, &wstatus, 0);  // Should correspond to raise(SIGSTOP)
  FAIL_IF(!WIFSTOPPED(wstatus) || WSTOPSIG(wstatus) != SIGSTOP) << "Unexpected stop from child";

  // Set up options to handle everything reliably. We do this before continuing
  // so that the actual running program has everything properly configured.
  int options = 0;
  options |= PTRACE_O_TRACEFORK | PTRACE_O_TRACECLONE | PTRACE_O_TRACEVFORK;  // Follow forks
  options |= PTRACE_O_TRACEEXEC;     // Handle execs more reliably
  options |= PTRACE_O_TRACESYSGOOD;  // When stepping through syscalls, be clear
  options |= PTRACE_O_TRACESECCOMP;  // Actually receive the syscall stops we requested

  FAIL_IF(ptrace(PTRACE_SETOPTIONS, child_pid, nullptr, options))
      << "Failed to set ptrace options: " << ERR;

  // Let the child restart to reach its exec.
  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  // Let the child finish its exec.
  waitpid(child_pid, &wstatus, 0);  // Should correspond to SECCOMP_RET_TRACE for execve
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8)))
      << "Unexpected stop from child. Expected SECCOMP";

  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  // Handle a stop from inside the exec
  waitpid(child_pid, &wstatus, 0);
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_EXEC << 8)))
      << "Unexpected stop from child. Expected EXEC";

  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  return child_pid;
}

void Tracer::handleSyscall(pid_t child) {
  struct user_regs_struct registers;
  FAIL_IF(ptrace(PTRACE_GETREGS, child, nullptr, &registers)) << "Failed to get registers: " << ERR;

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
      .path = "",
      .follow_links = true,
  };
  struct file_reference extra_file = {
      .fd = AT_FDCWD,
      .path = "",
      .follow_links = true,
  };

  bool file_created = false;

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

  // For open and openat, we need to know if the call created the file.
  if (registers.SYSCALL_NUMBER == __NR_open) {
    int flags = registers.SYSCALL_ARG2;
    if (flags & O_CREAT) {
      struct stat statbuf;
      file_created = (stat(main_file.path.c_str(), &statbuf) != 0);
    }
  } else if (registers.SYSCALL_NUMBER == __NR_openat) {
    int flags = registers.SYSCALL_ARG3;
    if (flags & O_CREAT) {
      struct stat statbuf;
      file_created = (stat(main_file.path.c_str(), &statbuf) != 0);
    }
  }

  // We need to know the return values of a few syscalls, specifically those that
  // allocate new fds. Therefore, use PTRACE_SYSCALL to step past just the syscall
  // and reinspect the process.
  switch (registers.SYSCALL_NUMBER) {
    case /* 2 */ __NR_open:
    case /* 257 */ __NR_openat:
    case /* 22 */ __NR_pipe:
    case /* 32 */ __NR_dup:
    case /* 72 */ __NR_fcntl:
    case /* 85 */ __NR_creat:
    case /* 293 */ __NR_pipe2:
      // We've already hit the syscall-enter, so wait for the syscall-exit.
      ptrace(PTRACE_SYSCALL, child, nullptr, 0);
      waitpid(child, nullptr, 0);  // FIXME: handle errors
      // PEEKUSER should hopefully be more efficient than GETREGS here because
      // we only care about a single register
      registers.SYSCALL_RETURN =
          ptrace(PTRACE_PEEKUSER, child, offsetof(struct user, regs.SYSCALL_RETURN), nullptr);
      if ((long long)registers.SYSCALL_RETURN < 0) {  // The call errored, so skip it
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
      pipe_fds[0] = read_tracee_data(child, registers.SYSCALL_ARG1);
      pipe_fds[1] = read_tracee_data(child, registers.SYSCALL_ARG1 + sizeof(int));
      break;
  }

  // Relaunch the child. After this point, we can no longer access the child's memory,
  // but we are allowed to do slightly more expensive things.
  // FIXME: We previously continued here, but this broke instances where we
  // needed a fingerprint. Contnuing at the end of the loop doesn't allow us to run
  // in parallel, so we want to continue as soon as possible. Therefore, once we have
  // any fingerprints we need, call continue immediately.

  // Handle fake-relative syscalls by ignoring the fd when the path is absolute
  if (main_file.path[0] == '/') {
    main_file.fd = AT_FDCWD;
  }
  if (extra_file.path[0] == '/') {
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

  // If this syscall only deals with something in /proc/self, then assume that it is messing
  // with process-internal state and we can safely ignore it.
  // FIXME: There are paths through /proc/self that lead out of procfs, such as
  // referencing a something in a directory linked by /proc/self (i.e.
  // /proc/self/fd/10/foo/bar)
  if (!main_file.follow_links && main_file.fd == AT_FDCWD && main_file.path.size() >= 11 &&
      main_file.path.substr(0, 11) == "/proc/self/") {
    ptrace(PTRACE_CONT, child, nullptr, 0);
    return;
  }

  // This is the giant switch statement where we handle what every syscall means. Note
  // that we don't handle execve and execveat, since the actual processing there is done
  // on STOP_EXEC (i.e. PTRACE_EVENT_EXEC).
  switch (registers.SYSCALL_NUMBER) {
    ////// Fiddling with file descriptors //////
    case /* 3 */ __NR_close:
      traceClose(child, registers.SYSCALL_ARG1);
      break;
    case /* 2 */ __NR_open:
      traceOpen(child,
                       registers.SYSCALL_RETURN,  // File descriptor
                       main_file.path,            // File path
                       file_created,              // Did the file exist before
                       registers.SYSCALL_ARG2,    // Flags
                       registers.SYSCALL_ARG3);   // File mode
      break;

    case /* 85 */ __NR_creat:
      traceOpen(child,
                       registers.SYSCALL_RETURN,      // File descriptor
                       main_file.path,                // File path
                       file_created,                  // Did the file exist before
                       O_CREAT | O_WRONLY | O_TRUNC,  // Flags
                       registers.SYSCALL_ARG2);       // File mode

      break;

    case /* 257 */ __NR_openat:
      traceOpen(child,
                       registers.SYSCALL_RETURN,  // File descriptor
                       main_file.path,            // File path
                       file_created,              // Did the file exist before
                       registers.SYSCALL_ARG3,    // Flags
                       registers.SYSCALL_ARG4);   // File mode
      break;

    case /* 22 */ __NR_pipe:
      tracePipe(child, pipe_fds, false);
      break;
    case /* 293 */ __NR_pipe2:
      tracePipe(child, pipe_fds, (registers.SYSCALL_ARG2 & O_CLOEXEC) != 0);
      break;
    case /* 32 */ __NR_dup:
      traceDup(child, registers.SYSCALL_ARG1, registers.SYSCALL_RETURN, false);
      break;
    case /* 33 */ __NR_dup2:
      traceDup(child, registers.SYSCALL_ARG1, registers.SYSCALL_ARG2, false);
      break;
    case /* 292 */ __NR_dup3:
      traceDup(child, registers.SYSCALL_ARG1, registers.SYSCALL_ARG2,
                      (registers.SYSCALL_ARG3 & O_CLOEXEC) != 0);
      break;
    case /* 72 */ __NR_fcntl:
      switch (registers.SYSCALL_ARG2) {
        case F_DUPFD:
          traceDup(child, registers.SYSCALL_ARG1, registers.SYSCALL_RETURN, false);
          break;
        case F_DUPFD_CLOEXEC:
          traceDup(child, registers.SYSCALL_ARG1, registers.SYSCALL_RETURN, true);
          break;
        case F_SETFD:
          traceSetCloexec(child, registers.SYSCALL_ARG1,
                                 (registers.SYSCALL_ARG3 & FD_CLOEXEC) != 0);
          break;
      }
      break;
    ////// Changing process state //////
    case /* 80 */ __NR_chdir:
    case /* 81 */ __NR_fchdir:
      traceChdir(child, main_file.path);
      break;
    case /* 161 */ __NR_chroot:
      traceChroot(child, main_file.path);
      break;
    ////// Complex operations /////
    case /* 9 */ __NR_mmap:
      traceMmap(child, main_file.fd);
      break;
    case /* 40 */ __NR_sendfile:
    case /* 275 */ __NR_splice:
    case /* 276 */ __NR_tee:
    case /* 326 */ __NR_copy_file_range:
      traceRead(child, main_file);
      traceModify(child, extra_file);
      break;
    case /* 76 */ __NR_truncate:
    case /* 77 */ __NR_ftruncate:
      if (registers.SYSCALL_ARG2 == 0) {
        traceTruncate(child, main_file);
      } else {
        traceModify(child, main_file);
      }
      break;
    case /* 82 */ __NR_rename:
    case /* 264 */ __NR_renameat:
    case /* 316 */ __NR_renameat2:  // TODO: Handle the flags
      traceRead(child, main_file);
      traceRemove(child, main_file);
      traceRemove(child, extra_file);
      traceCreate(child, extra_file);
      traceModify(child, extra_file);
      break;
    ////// Simple reads and writes //////
    case /* 0 */ __NR_read:
    case /* 17 */ __NR_pread64:
    case /* 19 */ __NR_readv:
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
      traceRead(child, main_file);
      break;

    ////// Do nothing for execve and execveat //////
    // These were being handled as reads, but that assigned an input dependency to the
    // wrong command. Only the command that is actually exec-ed depends on the contents
    // of the file. That happens in Process::traceExec().
    case /* 59 */ __NR_execve:
    case /* 322 */ __NR_execveat:
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
      traceModify(child, main_file);
      break;
    case /* 83 */ __NR_mkdir:
    case /* 88 */ __NR_symlink:
    case /* 133 */ __NR_mknod:
    case /* 258 */ __NR_mkdirat:
    case /* 259 */ __NR_mknodat:
    case /* 266 */ __NR_symlinkat:
      traceCreate(child, main_file);
      break;
    case /* 84 */ __NR_rmdir:
    case /* 87 */ __NR_unlink:
    case /* 263 */ __NR_unlinkat:
      traceRemove(child, main_file);
      break;
    default:
      WARN << "[" << child << "] Unhandled Syscall: " << (int)registers.SYSCALL_NUMBER;
      break;
  }

  // FIXME: See FIXME above about why this ought to be earlier in the loop
  FAIL_IF(ptrace(PTRACE_CONT, child, nullptr, 0)) << "Failed to resume child: " << ERR;
}

void Tracer::handleFork(pid_t child) {
  // GETEVENTMSG returns a unsigned long *always*, even though the value is
  // always a pid_t.
  unsigned long ul_new_child;
  FAIL_IF(ptrace(PTRACE_GETEVENTMSG, child, nullptr, &ul_new_child))
      << "Unable to fetch new child from fork: " << ERR;

  pid_t new_child = (pid_t)ul_new_child;

  FAIL_IF(ptrace(PTRACE_CONT, child, nullptr, 0)) << "Failed to resume child: " << ERR;

  traceFork(child, new_child);
}

void Tracer::handleExec(pid_t child) {
  struct user_regs_struct registers;
  FAIL_IF(ptrace(PTRACE_GETREGS, child, nullptr, &registers)) << "Failed to get registers: " << ERR;

  list<string> args;

  int child_argc = read_tracee_data(child, registers.rsp);
  for (int i = 0; i < child_argc; i++) {
    uintptr_t arg_ptr = read_tracee_data(child, registers.rsp + (1 + i) * sizeof(long));
    args.push_back(read_tracee_string(child, arg_ptr));
  }

  traceExec(child, get_executable(child), args);

  ptrace(PTRACE_CONT, child, nullptr, 0);
}

void Tracer::handleClone(pid_t child) {
  unsigned long ul_new_thread;
  FAIL_IF(ptrace(PTRACE_GETEVENTMSG, child, nullptr, &ul_new_thread))
      << "Unable to fetch new child from clone: " << ERR;

  pid_t new_thread = (pid_t)ul_new_thread;
  ptrace(PTRACE_CONT, child, nullptr, 0);
  // TODO handling of flags -> CLONE_FILES
  traceClone(child, new_thread);
}

void Tracer::handleExit(pid_t child) {
  traceExit(child);
}

void Tracer::run(Command* cmd) {
  string exec_path = cmd->getExecutable();

  vector<char*> exec_argv;

  // Are we running the root Dodofile?
  if (exec_path == "Dodofile") {
    // Will we be able to execute the Dodofile?
    if (faccessat(AT_FDCWD, exec_path.c_str(), X_OK, AT_EACCESS)) {
      // Execute would fail. Can we read it and run with sh?
      if (faccessat(AT_FDCWD, exec_path.c_str(), R_OK, AT_EACCESS)) {
        // No. Print an error.
        FAIL << "Unable to access Dodofile, which is required for the build.\n"
             << "See http://dodo.build for instructions.";
      }

      // Dodofile is readable but not executable. Run with sh by default.
      // Convert "Dodofile" to arg string
      exec_argv.push_back((char*)"Dodofile");

      // Replace exec path with sh
      exec_path = "/bin/sh";
    }
  }

  for (auto& arg : cmd->getArguments()) {
    exec_argv.push_back((char*)arg.c_str());
  }
  exec_argv.push_back(nullptr);
  pid_t pid = launch_traced(exec_path.c_str(), exec_argv.data(), {});

  // TODO: Fix cwd handling
  _processes[pid] = make_shared<Process>(pid, ".", cmd, cmd->getInitialFDs());

  while (true) {
    int wait_status;
    pid_t child = wait(&wait_status);
    if (child == -1) {
      if (errno == ECHILD) {
        // ECHILD is returned when there are no children to wait on, which
        // is by far the simplest and most reliable signal we have for when
        // to exit (cleanly).
        break;
      } else {
        FAIL << "Error while waiting: " << ERR;
      }
    }

    if (WIFSTOPPED(wait_status)) {
      int status = wait_status >> 8;

      if (status == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
        handleSyscall(child);
      
      } else if (status == (SIGTRAP | (PTRACE_EVENT_FORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_VFORK << 8))) {
        handleFork(child);
      
      } else if (status == (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        handleExec(child);
      
      } else if (status == (SIGTRAP | (PTRACE_EVENT_CLONE << 8))) {
        handleClone(child);
      
      } else {
        WARN << "Unhandled stop in process " << child;
        // We don't bother handling errors here, because any failure
        // just means that the child is somehow broken, and we shouldn't
        // continue processing it anyway.
        ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
      }
    
    } else if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
      handleExit(child);
    }
  }
}

void Tracer::traceChdir(pid_t pid, string path) {
  _processes[pid]->_cwd = path;
}

void Tracer::traceChroot(pid_t pid, string path) {
  _processes[pid]->_root = path;
}

void Tracer::traceFork(pid_t pid, pid_t child_pid) {
  auto proc = _processes[pid];
  auto child_proc = make_shared<Process>(child_pid, proc->_cwd, proc->_command, proc->_fds);
  _processes[child_pid] = child_proc;
}

void Tracer::traceClone(pid_t pid, pid_t thread_id) {
  // Threads in the same process just appear as pid references to the same process
  _processes[thread_id] = _processes[pid];
}

void Tracer::traceExec(pid_t pid, string executable, const list<string>& args) {
  auto proc = _processes[pid];

  // Close all cloexec file descriptors
  for (auto fd_entry = proc->_fds.begin(); fd_entry != proc->_fds.end();) {
    if (fd_entry->second.cloexec) {
      fd_entry = proc->_fds.erase(fd_entry);
    } else {
      ++fd_entry;
    }
  }

  // Create the new command
  proc->_command = proc->_command->createChild(executable, args, proc->_fds);

  // The new command depends on its executable file
  _graph.getFile(executable)->readBy(proc->_command);
}

void Tracer::traceExit(pid_t pid) {}

void Tracer::traceOpen(pid_t pid, int fd, string path, bool file_created, int flags, mode_t mode) {
  auto f = _graph.getFile(path);
  auto proc = _processes[pid];

  int access_mode = flags & (O_RDONLY | O_WRONLY | O_RDWR);
  bool cloexec = (flags & O_CLOEXEC) != 0;

  // Record the file descriptor entry
  proc->_fds[fd] = FileDescriptor(f, access_mode, cloexec);

  if (file_created) {
    f->createdBy(proc->_command);
  } else if (flags & O_TRUNC) {
    f->truncatedBy(proc->_command);
  }

  // Otherwise, no interaction yet until we do something with the file

  LOG << proc->_command << " opened " << f;
}

void Tracer::traceClose(pid_t pid, int fd) {
  auto proc = _processes[pid];
  auto f = proc->_fds[fd].file;

  // Log the event if there was actually a valid file descriptor
  if (f) LOG << proc->_command << " closed " << proc->_fds[fd].file;

  proc->_fds.erase(fd);
}

void Tracer::tracePipe(pid_t pid, int fds[2], bool cloexec) {
  auto proc = _processes[pid];
  auto f = _graph.getPipe();

  f->createdBy(proc->_command);

  proc->_fds[fds[0]] = FileDescriptor(f, O_RDONLY, cloexec);
  proc->_fds[fds[1]] = FileDescriptor(f, O_WRONLY, cloexec);
}

void Tracer::traceDup(pid_t pid, int duped_fd, int new_fd, bool cloexec) {
  auto proc = _processes[pid];

  auto target_fd = proc->_fds.find(new_fd);
  if (target_fd != proc->_fds.end()) {
    LOG << proc->_command << " closed " << proc->_fds[new_fd].file << " via dup";
  }

  auto duped_file = proc->_fds.find(duped_fd);
  if (duped_file == proc->_fds.end()) {
    proc->_fds.erase(new_fd);
  } else {
    proc->_fds[new_fd] = proc->_fds[duped_fd];
  }
}

void Tracer::traceSetCloexec(pid_t pid, int fd, bool cloexec) {
  auto proc = _processes[pid];
  auto file = proc->_fds.find(fd);
  if (file != proc->_fds.end()) {
    file->second.cloexec = cloexec;
  }
}

void Tracer::traceMmap(pid_t pid, int fd) {
  // TODO
}

File* Tracer::resolveFileRef(shared_ptr<Process> proc, struct file_reference& file) {
  File* f = nullptr;
  if (file.fd == AT_FDCWD) {
    f = _graph.getFile(file.path);
  } else {
    f = proc->_fds[file.fd].file;
  }

  if (!f) {
    WARN << "Unable to resolve file in " << proc->_command;
    WARN << "  fd: " << file.fd;
    WARN << "  path: " << file.path;
    WARN << "FDs:";
    for (auto entry : proc->_fds) {
      int index = entry.first;
      auto file = entry.second.file;

      if (file) {
        WARN << "  " << index << ": " << file;
      } else {
        WARN << "  " << index << ": INVALID";
      }
    }
  }

  return f;
}

void Tracer::traceRead(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->readBy(proc->_command);
}

void Tracer::traceModify(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->writtenBy(proc->_command);
}

void Tracer::traceTruncate(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->truncatedBy(proc->_command);
}

void Tracer::traceCreate(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->createdBy(proc->_command);
}

void Tracer::traceRemove(pid_t pid, struct file_reference& file) {
  auto proc = _processes[pid];
  auto f = resolveFileRef(proc, file);
  if (f) f->deletedBy(proc->_command);
}
