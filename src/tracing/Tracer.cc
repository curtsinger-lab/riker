#include "Tracer.hh"

#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <list>
#include <memory>
#include <utility>
#include <vector>

#include <linux/bpf_common.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <signal.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <unistd.h>

#include "build/Build.hh"
#include "data/Command.hh"
#include "data/Version.hh"
#include "tracing/FDEntry.hh"
#include "tracing/Process.hh"
#include "tracing/syscalls.hh"
#include "ui/log.hh"

using std::dynamic_pointer_cast;
using std::list;
using std::make_shared;
using std::pair;
using std::shared_ptr;

void Tracer::run(shared_ptr<Command> cmd) {
  // Launch the command with tracing
  launchTraced(cmd);

  // Sometime we get tracing events before we can process them. This queue holds that list
  list<pair<pid_t, int>> event_queue;

  // Process tracaing events
  while (true) {
    int wait_status;
    pid_t child;

    // Do we have an event to process?
    bool have_event = false;

    // Check if any queued events are ready to be processed
    for (auto iter = event_queue.begin(); iter != event_queue.end(); iter++) {
      // If the current entry's pid is now a known process, process it
      if (_processes.find(iter->first) != _processes.end()) {
        // Pull out the child and status values
        child = iter->first;
        wait_status = iter->second;

        // Drop the event from the queue
        event_queue.erase(iter);

        // We have an event now
        have_event = true;
        break;
      }
    }

    // If we didn't pull an event from the queue, wait for one
    while (!have_event) {
      child = wait(&wait_status);

      // Handle errors
      if (child == -1) {
        // If errno is ECHILD, we're done and can return
        if (errno == ECHILD)
          return;
        else
          FAIL << "Error while waiting: " << ERR;
      }

      // Does this event refer to a process we don't know about yet?
      if (_processes.find(child) == _processes.end()) {
        // Yes. Queue the event so we can try another one.
        event_queue.emplace_back(child, wait_status);
      } else {
        // No. The event is for a known process, so we'll deal with it right now
        have_event = true;
      }
    }

    auto p = _processes[child];

    if (WIFSTOPPED(wait_status)) {
      int status = wait_status >> 8;

      if (status == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
        // Stopped on entry to a syscall
        handleSyscall(p);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_FORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_VFORK << 8))) {
        // TODO: Is this called in the child just after fork()?
        handleFork(p);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        // TODO: Is this called before or after exec?
        FAIL << "handleExec is gone. I thought we wouldn't need it?";

      } else if (status == (SIGTRAP | (PTRACE_EVENT_CLONE << 8))) {
        // TODO: Is this called in the child just after clone()?
        auto regs = p->getRegisters();
        handleClone(p, regs.SYSCALL_ARG1);

      } else {
        // The traced process received a signal. Just pass it along.
        ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
      }

    } else if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
      // Stopped on exit
      handleExit(p);
    }
  }
}

// Launch a program fully set up with ptrace and seccomp to be traced by the current process.
// launch_traced will return the PID of the newly created process, which should be running (or at
// least ready to be waited on) upon return.
void Tracer::launchTraced(shared_ptr<Command> cmd) {
  LOG << "Launching " << cmd;

  // Fill this vector in with {parent_fd, child_fd} pairs
  // The launched child will dup2 these into place
  vector<pair<int, int>> initial_fds;

  LOG << "Initial FDs:";

  // Loop over the initial fds for the command we are launching
  for (auto& [child_fd, info] : cmd->getInitialFDs()) {
    LOG << "  " << child_fd << ": " << info.getReference();

    // For now, only handle Access references
    auto ref = dynamic_pointer_cast<Access>(info.getReference());
    if (!ref) continue;

    // Get the artifact from the environment
    auto [artifact, rc, created] = _build.getEnv().getFile(cmd, ref);

    // Open the artifact the command expects in its FD table. Right now, this logic assumes either:
    // 1. The open() call will create/truncate the file, or
    // 2. The file is in the correct state on disk becuase it was created/written by other commands.
    // This will be more complex once we have the possibility of cached files that need to be moved
    // into place. The logic to prepare these files (and any directories they may depend on) should
    // go into the Env/Artifact classes.
    int parent_fd = ref->open();
    FAIL_IF(parent_fd < 0) << "Failed to open reference " << ref;
    initial_fds.emplace_back(parent_fd, child_fd);
  }

  // In terms of overall structure, this is a bog standard fork/exec spawning function.
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
    for (auto [parent_fd, child_fd] : initial_fds) {
      int rc = dup2(parent_fd, child_fd);

      LOG << "Duped parent fd " << parent_fd << " to " << child_fd;

      FAIL_IF(rc != child_fd) << "Failed to initialize fds: " << ERR;
    }

    // Allow ourselves to be traced by our parent
    FAIL_IF(ptrace(PTRACE_TRACEME, 0, nullptr, nullptr) != 0) << "Failed to start tracing: " << ERR;

    // Lock down the process so that we are allowed to
    // use seccomp without special permissions
    FAIL_IF(prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) << "Failed to allow seccomp: " << ERR;

    vector<struct sock_filter> filter;

    // Load the syscall number
    filter.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr)));

    // Loop over syscalls
    for (auto& entry : syscalls) {
      uint32_t syscall_nr = entry.first;
      // Check if the syscall matches the current entry
      filter.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, syscall_nr, 0, 1));

      // On a match, return trace
      filter.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_TRACE));
    }

    // Default case allows the syscall
    filter.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW));

    struct sock_fprog bpf_program;
    bpf_program.filter = filter.data();
    bpf_program.len = filter.size();

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

    vector<const char*> args;
    for (auto& s : cmd->getArguments()) {
      args.push_back(s.c_str());
    }

    // Null-terminate the args array
    args.push_back(nullptr);

    // TODO: explicitly handle the environment
    execv(cmd->getExecutable().c_str(), (char* const*)args.data());

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

  // Handle a stop on entry to the exec
  waitpid(child_pid, &wstatus, 0);
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8)))
      << "Unexpected stop from child. Expected SECCOMP";

  // Let the child continue with its exec
  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  // Handle a stop from inside the exec
  waitpid(child_pid, &wstatus, 0);
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_EXEC << 8)))
      << "Unexpected stop from child. Expected EXEC";

  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  // Build the file descriptor table for the running process
  map<int, FDEntry> fds;

  // Loop over the references the command expects to have in its FD table
  for (auto& [index, initial_fd] : cmd->getInitialFDs()) {
    auto ref = initial_fd.getReference();
    auto [artifact, rc, created] = _build.getEnv().get(cmd, ref);

    FAIL_IF(!artifact || rc != SUCCESS) << "Failed to get artifact for initial file descriptor";

    fds.emplace(index, FDEntry(ref, artifact, initial_fd.isWritable()));
  }

  _processes[child_pid] = make_shared<Process>(_build, child_pid, ".", cmd, fds);
}

void Tracer::handleClone(shared_ptr<Process> p, int flags) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new thread id and then resume execution
  pid_t new_pid = p->getEventMessage();
  p->resume();

  // TODO: Handle flags

  // Threads in the same process just appear as pid references to the same process
  _processes[new_pid] = _processes[p->_pid];
}

void Tracer::handleFork(shared_ptr<Process> p) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new process id and resume execution
  pid_t new_pid = p->getEventMessage();
  p->resume();

  // If the call failed, do nothing
  if (new_pid == -1) return;

  LOG << "fork called in " << p;

  // Create a new process running the same command
  auto new_proc = make_shared<Process>(_build, new_pid, p->_cwd, p->_command, p->_fds);
  _processes[new_pid] = new_proc;

  LOG << "new process " << new_proc;
}

void Tracer::handleExit(shared_ptr<Process> p) {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Remove the process. No need to resume, since the process has exited
  _processes.erase(p->_pid);
}

void Tracer::handleSyscall(shared_ptr<Process> p) {
  auto regs = p->getRegisters();

  // This giant switch statement invokes the appropriate system call handler on a traced
  // process after decoding the syscall arguments.
  switch (regs.SYSCALL_NUMBER) {
    case __NR_execve:
      p->_execve(p->readString(regs.SYSCALL_ARG1), p->readArgvArray(regs.SYSCALL_ARG2),
                 p->readArgvArray(regs.SYSCALL_ARG3));
      break;

    case __NR_execveat:
      p->_execveat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2),
                   p->readArgvArray(regs.SYSCALL_ARG3), p->readArgvArray(regs.SYSCALL_ARG4));
      break;

    case __NR_stat:
      p->_stat(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_newfstatat:
      p->_fstatat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG4);
      break;

    case __NR_fstat:
      p->_fstat(regs.SYSCALL_ARG1);
      break;

    case __NR_lstat:
      p->_lstat(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_access:
      p->_access(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_faccessat:
      p->_faccessat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                    regs.SYSCALL_ARG4);
      break;

    case __NR_read:
      p->_read(regs.SYSCALL_ARG1);
      break;

    case __NR_write:
      p->_write(regs.SYSCALL_ARG1);
      break;

    case __NR_open:
      p->_open(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_close:
      p->_close(regs.SYSCALL_ARG1);
      break;

    case __NR_mmap:
      p->_mmap((void*)regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3, regs.SYSCALL_ARG4,
               regs.SYSCALL_ARG5, regs.SYSCALL_ARG6);
      break;

    case __NR_pread64:
      p->_pread64(regs.SYSCALL_ARG1);
      break;

    case __NR_pwrite64:
      p->_pwrite64(regs.SYSCALL_ARG1);
      break;

    case __NR_readv:
      p->_readv(regs.SYSCALL_ARG1);
      break;

    case __NR_writev:
      p->_writev(regs.SYSCALL_ARG1);
      break;

    case __NR_pipe:
      p->_pipe((int*)regs.SYSCALL_ARG1);
      break;

    case __NR_dup:
      p->_dup(regs.SYSCALL_ARG1);
      break;

    case __NR_dup2:
      p->_dup2(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_sendfile:
      p->_sendfile(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_fcntl:
      p->_fcntl(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_truncate:
      p->_truncate(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_ftruncate:
      p->_ftruncate(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_getdents:
      p->_getdents(regs.SYSCALL_ARG1);
      break;

    case __NR_chdir:
      p->_chdir(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_fchdir:
      p->_fchdir(regs.SYSCALL_ARG1);
      break;

    case __NR_rename:
      p->_rename(p->readString(regs.SYSCALL_ARG1), p->readString(regs.SYSCALL_ARG2));
      break;

    case __NR_mkdir:
      p->_mkdir(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_rmdir:
      p->_rmdir(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_creat:
      p->_creat(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_unlink:
      p->_unlink(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_symlink:
      p->_symlink(p->readString(regs.SYSCALL_ARG1), p->readString(regs.SYSCALL_ARG2));
      break;

    case __NR_readlink:
      p->_readlink(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_chmod:
      p->_chmod(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2);
      break;

    case __NR_fchmod:
      p->_fchmod(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_chown:
      p->_chown(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_fchown:
      p->_fchown(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_lchown:
      p->_lchown(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_mknod:
      p->_mknod(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_chroot:
      p->_chroot(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_setxattr:
      p->_setxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_lsetxattr:
      p->_lsetxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_fsetxattr:
      p->_fsetxattr(regs.SYSCALL_ARG1);
      break;

    case __NR_getxattr:
      p->_getxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_lgetxattr:
      p->_lgetxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_fgetxattr:
      p->_fgetxattr(regs.SYSCALL_ARG1);
      break;

    case __NR_listxattr:
      p->_listxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_llistxattr:
      p->_llistxattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_flistxattr:
      p->_flistxattr(regs.SYSCALL_ARG1);
      break;

    case __NR_removexattr:
      p->_removexattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_lremovexattr:
      p->_lremovexattr(p->readString(regs.SYSCALL_ARG1));
      break;

    case __NR_fremovexattr:
      p->_fremovexattr(regs.SYSCALL_ARG1);
      break;

    case __NR_getdents64:
      p->_getdents64(regs.SYSCALL_ARG1);
      break;

    case __NR_openat:
      p->_openat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                 regs.SYSCALL_ARG4);
      break;

    case __NR_mkdirat:
      p->_mkdirat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3);
      break;

    case __NR_mknodat:
      p->_mknodat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                  regs.SYSCALL_ARG4);
      break;

    case __NR_fchownat:
      p->_fchownat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                   regs.SYSCALL_ARG4, regs.SYSCALL_ARG5);
      break;

    case __NR_unlinkat:
      p->_unlinkat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3);
      break;

    case __NR_renameat:
      p->_renameat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                   p->readString(regs.SYSCALL_ARG4));
      break;

    case __NR_symlinkat:
      p->_symlinkat(p->readString(regs.SYSCALL_ARG1), regs.SYSCALL_ARG2,
                    p->readString(regs.SYSCALL_ARG3));
      break;

    case __NR_readlinkat:
      p->_readlinkat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2));
      break;

    case __NR_fchmodat:
      p->_fchmodat(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                   regs.SYSCALL_ARG4);
      break;

    case __NR_splice:
      p->_splice(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3, regs.SYSCALL_ARG4);
      break;

    case __NR_tee:
      p->_tee(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2 /*, regs.SYSCALL_ARG3*/);  // Omitting length
      break;

    case __NR_vmsplice:
      p->_vmsplice(regs.SYSCALL_ARG1);
      break;

    case __NR_dup3:
      p->_dup3(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_pipe2:
      p->_pipe2((int*)regs.SYSCALL_ARG1, regs.SYSCALL_ARG2);
      break;

    case __NR_preadv:
      p->_preadv(regs.SYSCALL_ARG1);
      break;

    case __NR_pwritev:
      p->_pwritev(regs.SYSCALL_ARG1);
      break;

    case __NR_renameat2:
      p->_renameat2(regs.SYSCALL_ARG1, p->readString(regs.SYSCALL_ARG2), regs.SYSCALL_ARG3,
                    p->readString(regs.SYSCALL_ARG4), regs.SYSCALL_ARG5);
      break;

    case __NR_copy_file_range:
      p->_copy_file_range(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    case __NR_preadv2:
      p->_preadv2(regs.SYSCALL_ARG1);
      break;

    case __NR_pwritev2:
      p->_pwritev2(regs.SYSCALL_ARG1);
      break;

    case __NR_lseek:
      p->_lseek(regs.SYSCALL_ARG1, regs.SYSCALL_ARG2, regs.SYSCALL_ARG3);
      break;

    default:
      auto iter = syscalls.find(regs.SYSCALL_NUMBER);
      if (iter == syscalls.end()) {
        FAIL << "Unexpected system call number: " << regs.SYSCALL_NUMBER;
      } else {
        WARN << "Missing case for syscall: " << syscalls[regs.SYSCALL_NUMBER];
        p->resume();
      }
  }
}
