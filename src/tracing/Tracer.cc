#include "Tracer.hh"

#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <list>
#include <memory>
#include <set>
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

#include "artifacts/PipeArtifact.hh"
#include "build/Build.hh"
#include "core/Command.hh"
#include "core/FileDescriptor.hh"
#include "tracing/Process.hh"
#include "tracing/SyscallTable.hh"
#include "util/log.hh"
#include "versions/Version.hh"

using std::list;
using std::make_shared;
using std::pair;
using std::set;
using std::shared_ptr;

/// A single global instance of this class is used to clean up tracers when there is a fatal error
class TracerCleanup {
 public:
  /// Run cleanup actions for all known tracers
  ~TracerCleanup() {
    for (auto t : _tracers) {
      t->cleanup();
    }
  }

  /// Add a tracer to the set of tracers to clean up on exit
  void add(Tracer* t) { _tracers.emplace(t); }

  /// Remove a tracer from the set of tracers to clean up on exit
  void remove(Tracer* t) { _tracers.erase(t); }

 private:
  set<Tracer*> _tracers;
};

TracerCleanup cleaner;

// Create a tracer and keep a record of it in the cleanup object
Tracer::Tracer(Build& build) noexcept : _build(build) {
  cleaner.add(this);
}

// Remove this tracers from the cleanup object when it is destroyed
Tracer::~Tracer() noexcept {
  cleaner.remove(this);
}

// Clean up any processes left in this tracer
void Tracer::cleanup() noexcept {
  for (auto [pid, process] : _processes) {
    kill(pid, SIGKILL);
  }
}

shared_ptr<Process> Tracer::start(shared_ptr<Command> cmd) noexcept {
  // Launch the command with tracing
  return launchTraced(cmd);
}

void Tracer::wait(shared_ptr<Process> p) noexcept {
  // Process events until the given command has exited
  // Sometime we get tracing events before we can process them. This queue holds that list
  list<pair<pid_t, int>> event_queue;

  // Process tracaing events
  while (true) {
    // If we're waiting for a specific process, and that process has exited, return now
    if (p && p->hasExited()) return;

    int wait_status;
    pid_t child;

    // Do we have an event to process?
    bool have_event = false;

    // Check if any queued events are ready to be processed
    for (auto iter = event_queue.cbegin(); iter != event_queue.cend(); iter++) {
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
      child = ::wait(&wait_status);

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

    const auto& p = _processes[child];

    if (WIFSTOPPED(wait_status)) {
      int status = wait_status >> 8;

      if (status == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
        // Stopped on entry to a syscall
        try {
          handleSyscall(p);
        } catch (int& i) {
          p->resume();
        }

      } else if (status == (SIGTRAP | (PTRACE_EVENT_FORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_VFORK << 8))) {
        // TODO: Is this called in the child just after fork()?
        handleFork(p);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_CLONE << 8))) {
        // TODO: Is this called in the child just after clone()?
        auto regs = p->getRegisters();
        handleClone(p, regs.SYSCALL_ARG1);

      } else if (status == (SIGTRAP | 0x80)) {
        // This is a stop at the end of a system call that was resumed.
        p->syscallFinished();

      } else if (status == (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        // This is a stop after an exec finishes. The process that called exec must have set a
        // post-syscall handler
        p->syscallFinished();

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

void Tracer::handleClone(shared_ptr<Process> p, int flags) noexcept {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new thread id and then resume execution
  pid_t new_pid = p->getEventMessage();
  p->resume();

  // TODO: Handle flags

  // Threads in the same process just appear as pid references to the same process
  _processes[new_pid] = _processes[p->_pid];
}

void Tracer::handleFork(shared_ptr<Process> p) noexcept {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new process id and resume execution
  pid_t new_pid = p->getEventMessage();
  p->resume();

  // If the call failed, do nothing
  if (new_pid == -1) return;

  // Create a new process running the same command
  _processes[new_pid] = p->fork(new_pid);
}

void Tracer::handleExit(shared_ptr<Process> p) noexcept {
  _processes.erase(p->_pid);
  p->setExited();
  _exited.emplace(p->_pid, p);
}

shared_ptr<Process> Tracer::getExited(pid_t pid) noexcept {
  auto result = _exited[pid];
  _exited.erase(pid);
  return result;
}

void Tracer::handleSyscall(shared_ptr<Process> p) noexcept {
  auto regs = p->getRegisters();

  const auto& entry = SyscallTable::get(regs.SYSCALL_NUMBER);
  if (entry.isTraced()) {
    entry.runHandler(p, regs);
  } else {
    WARN << "Unexpected system call number " << regs.SYSCALL_NUMBER;
    p->resume();
  }
}

// Launch a program fully set up with ptrace and seccomp to be traced by the current process.
// launch_traced will return the PID of the newly created process, which should be running (or at
// least ready to be waited on) upon return.
shared_ptr<Process> Tracer::launchTraced(shared_ptr<Command> cmd) noexcept {
  LOG << "Launching " << cmd;

  // Get a reference to the directory where the command will be started
  auto cwd = cmd->getInitialWorkingDir();

  // Get a reference to the root directory in effect when the command is started
  auto root = cmd->getInitialRootDir();

  // Fill this vector in with {parent_fd, child_fd} pairs
  // The launched child will dup2 these into place
  vector<pair<int, int>> initial_fds;

  LOG << "Initial FDs:";

  // Loop over the initial fds for the command we are launching
  for (const auto& [child_fd, info] : cmd->getInitialFDs()) {
    LOG << "  " << child_fd << ": " << info.getReference();

    // Make sure the reference has already been resolved
    ASSERT(info.getReference()->isResolved())
        << "Tried to launch a command with an unresolved reference in its "
           "initial file descriptor table";

    // Handle reference types
    if (auto ref = info.getReference()->as<Access>()) {
      // This is an access, so we have a path

      // Commit any emulated modifications to this artifact to the filesystem
      ref->getArtifact()->commit(ref);

      // Use the reference to open the file
      int parent_fd = ref->open();
      FAIL_IF(parent_fd < 0) << "Failed to open reference " << ref;
      initial_fds.emplace_back(parent_fd, child_fd);

    } else if (auto ref = info.getReference()->as<Pipe>()) {
      // This is a pipe. Get the artifact.
      auto pipe = ref->getArtifact()->as<PipeArtifact>();

      // Does the descriptor refer to the writing end of the pipe?
      if (info.isWritable()) {
        initial_fds.emplace_back(pipe->getWriteFD(), child_fd);
      } else {
        initial_fds.emplace_back(pipe->getReadFD(), child_fd);
      }

    } else {
      WARN << "Skipping expected file descriptor " << child_fd << ": " << info;
    }
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

  // Close FDs in the parent
  if (child_pid > 0) {
    for (auto [parent_fd, child_fd] : initial_fds) {
      if (parent_fd != child_fd) close(parent_fd);
    }
  }

  if (child_pid == 0) {
    // This is the child

    // Set up FDs as requested. We assume that all parent FDs are marked CLOEXEC if
    // necessary and that there are no ordering constraints on duping (e.g. if the
    // child fd for one entry matches the parent fd of another).
    for (const auto& [parent_fd, child_fd] : initial_fds) {
      int rc = dup2(parent_fd, child_fd);

      LOG << "Duped parent fd " << parent_fd << " to " << child_fd;

      FAIL_IF(rc != child_fd) << "Failed to initialize fds: " << ERR;
    }

    // TODO: Change to the appropriate working directory
    // TODO: Change to the appropriate root directory

    // Allow ourselves to be traced by our parent
    FAIL_IF(ptrace(PTRACE_TRACEME, 0, nullptr, nullptr) != 0) << "Failed to start tracing: " << ERR;

    // Lock down the process so that we are allowed to
    // use seccomp without special permissions
    FAIL_IF(prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) << "Failed to allow seccomp: " << ERR;

    vector<struct sock_filter> filter;

    // Load the syscall number
    filter.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr)));

    // Loop over syscalls
    for (uint32_t i = 0; i < SyscallTable::size(); i++) {
      if (SyscallTable::get(i).isTraced()) {
        // Check if the syscall matches the current entry
        filter.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, i, 0, 1));

        // On a match, return trace
        filter.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_TRACE));
      }
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
    for (const auto& s : cmd->getArguments()) {
      args.push_back(s.c_str());
    }

    // Null-terminate the args array
    args.push_back(nullptr);

    // TODO: explicitly handle the environment
    execv(cmd->getExecutable()->getFullPath().c_str(), (char* const*)args.data());

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
  options |= PTRACE_O_TRACEEXIT;     // Trace exits

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

  auto p = make_shared<Process>(_build, *this, cmd, child_pid, cwd, root, cmd->getInitialFDs());

  _processes[child_pid] = p;

  return p;
}
