#include "Tracer.hh"

#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <tuple>
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

#include "artifacts/FileArtifact.hh"
#include "artifacts/PipeArtifact.hh"
#include "data/FileDescriptor.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "tracing/Process.hh"
#include "tracing/SyscallTable.hh"
#include "tracing/Thread.hh"
#include "util/log.hh"
#include "versions/Version.hh"

using std::list;
using std::make_shared;
using std::map;
using std::nullopt;
using std::optional;
using std::set;
using std::shared_ptr;
using std::tuple;

string signalName(int sig) {
  static map<int, string> signals{
      {SIGHUP, "SIGHUP"},       {SIGINT, "SIGINT"},       {SIGQUIT, "SIGQUIT"},
      {SIGILL, "SIGILL"},       {SIGTRAP, "SIGTRAP"},     {SIGABRT, "SIGABRT"},
      {SIGIOT, "SIGIOT"},       {SIGBUS, "SIGBUS"},       {SIGFPE, "SIGFPE"},
      {SIGKILL, "SIGKILL"},     {SIGUSR1, "SIGUSR1"},     {SIGSEGV, "SIGSEGV"},
      {SIGUSR2, "SIGUSR2"},     {SIGPIPE, "SIGPIPE"},     {SIGALRM, "SIGALRM"},
      {SIGTERM, "SIGTERM"},     {SIGSTKFLT, "SIGSTKFLT"}, {SIGCHLD, "SIGCHLD"},
      {SIGCLD, "SIGCLD"},       {SIGCONT, "SIGCONT"},     {SIGSTOP, "SIGSTOP"},
      {SIGTSTP, "SIGTSTP"},     {SIGTTIN, "SIGTTIN"},     {SIGTTOU, "SIGTTOU"},
      {SIGURG, "SIGURG"},       {SIGXCPU, "SIGXCPU"},     {SIGXFSZ, "SIGXFSZ"},
      {SIGVTALRM, "SIGVTALRM"}, {SIGPROF, "SIGPROF"},     {SIGWINCH, "SIGWINCH"},
      {SIGIO, "SIGIO"},         {SIGPOLL, "SIGPOLL"},     {SIGPWR, "SIGPWR"},
      {SIGSYS, "SIGSYS"}};

  auto iter = signals.find(sig);
  if (iter == signals.end()) return std::to_string(sig);
  return iter->second;
}

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
  for (auto [tid, thread] : _threads) {
    if (tid != gettid()) {
      tgkill(thread->getProcess()->getID(), tid, SIGKILL);
    }
  }
}

shared_ptr<Process> Tracer::start(shared_ptr<Command> cmd) noexcept {
  // Launch the command with tracing
  return launchTraced(cmd);
}

optional<tuple<pid_t, int>> Tracer::getEvent(bool block) noexcept {
  // Check if any queued events are ready to be processed
  for (auto iter = _event_queue.cbegin(); iter != _event_queue.cend(); iter++) {
    auto [child, wait_status] = *iter;

    // If the current entry's pid is now a known process, process it
    if (_threads.find(child) != _threads.end()) {
      // Drop the event from the queue
      _event_queue.erase(iter);

      // Return the event
      return tuple{child, wait_status};
    }
  }

  // If blocking is not allowed, there is no event to return
  if (!block) return nullopt;

  // Wait for an event from ptrace
  while (true) {
    int wait_status;
    pid_t child = ::wait(&wait_status);

    // Handle errors
    if (child == -1) {
      // If errno is ECHILD, we're done and can return with no event
      if (errno == ECHILD)
        return nullopt;
      else
        FAIL << "Error while waiting: " << ERR;
    }

    // Does this event refer to a process we don't know about yet?
    if (_threads.find(child) == _threads.end()) {
      // Yes. Queue the event so we can try another one.
      _event_queue.emplace_back(child, wait_status);
    } else {
      // No. The event is for a known process. Return it now.
      return tuple{child, wait_status};
    }
  }
}

void Tracer::wait(shared_ptr<Process> p) noexcept {
  if (p) {
    LOG(exec) << "Waiting for " << p;
  } else {
    LOG(exec) << "Waiting for all remaining processes";
  }

  // Process tracaing events
  while (true) {
    // If we're waiting for a specific process, and that process has exited, return now
    if (p && p->hasExited()) return;

    auto e = getEvent();
    if (!e.has_value()) return;

    auto [child, wait_status] = e.value();

    const auto& thread = _threads[child];

    if (WIFSTOPPED(wait_status)) {
      int status = wait_status >> 8;

      if (status == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
        // Stopped on entry to a syscall
        handleSyscall(thread);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_FORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_VFORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_FORK << 8) | (PTRACE_EVENT_VFORK << 8))) {
        handleFork(thread);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_CLONE << 8))) {
        auto regs = thread->getRegisters();
        handleClone(thread, regs.SYSCALL_ARG1);

      } else if (status == (SIGTRAP | 0x80)) {
        // This is a stop at the end of a system call that was resumed.
        thread->syscallFinished();

      } else if (status == (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        // This is a stop after an exec finishes. The process that called exec must have set a
        // post-syscall handler
        thread->syscallFinished();

      } else if (WSTOPSIG(wait_status) == SIGSTOP || WSTOPSIG(wait_status) == SIGTSTP ||
                 WSTOPSIG(wait_status) == SIGTTIN || WSTOPSIG(wait_status) == SIGTTOU) {
        // The tracee was stopped by a stopping signal (one of the four above).
        // This is either a signal delivery, or a group stop. We can find out by trying to get the
        // signal information. That call will fail for group-stop signals.
        siginfo_t info;
        int rc = ptrace(PTRACE_GETSIGINFO, child, nullptr, &info);
        if (rc == -1) {
          LOG(trace) << thread << " in group-stop";
          ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));

        } else {
          LOG(trace) << thread << ": injecting signal " << signalName(WSTOPSIG(wait_status))
                     << " (status=" << status << ")";
          ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
        }

      } else {
        // The traced process received a signal. Just pass it along.
        LOG(trace) << thread << ": injecting signal " << signalName(WSTOPSIG(wait_status))
                   << " (status=" << status << ")";
        ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
      }

    } else if (WIFEXITED(wait_status) || WIFSIGNALED(wait_status)) {
      // Stopped on exit
      handleExit(thread);
    }
  }
}

void Tracer::handleClone(shared_ptr<Thread> t, int flags) noexcept {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new thread id and then resume execution
  pid_t new_tid = t->getEventMessage();
  t->resume();

  // TODO: Handle flags

  // Threads in the same process just appear as pid references to the same process
  _threads[new_tid] = make_shared<Thread>(_build, *this, t->getProcess(), new_tid);
}

void Tracer::handleFork(shared_ptr<Thread> t) noexcept {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new process id and resume execution
  pid_t new_pid = t->getEventMessage();
  t->resume();

  // If the call failed, do nothing
  if (new_pid == -1) return;

  // Create a new process running the same command
  auto new_proc = t->getProcess()->fork(new_pid);

  // Record a new thread running in this process. It is the main thread, so pid and tid will be
  // equal
  _threads[new_pid] = make_shared<Thread>(_build, *this, new_proc, new_pid);
}

void Tracer::handleExit(shared_ptr<Thread> t) noexcept {
  LOGF(trace, "{}: exited", t);

  _threads.erase(t->getID());

  // Is the thread that's exiting the main thread in its process?
  auto proc = t->getProcess();
  if (t->getID() == proc->getID()) {
    LOGF(trace, "{}: exited", proc);
    proc->exit();
    _exited.emplace(proc->getID(), proc);
  }
}

shared_ptr<Process> Tracer::getExited(pid_t pid) noexcept {
  auto result = _exited[pid];
  _exited.erase(pid);
  return result;
}

void Tracer::handleSyscall(shared_ptr<Thread> t) noexcept {
  auto regs = t->getRegisters();

  const auto& entry = SyscallTable::get(regs.SYSCALL_NUMBER);
  if (entry.isTraced()) {
    LOG(trace) << t << ": stopped on syscall " << entry.getName();
    entry.runHandler(t, regs);
  } else {
    FAIL << "Traced system call number " << regs.SYSCALL_NUMBER << " in " << t;
  }
}

// Launch a program fully set up with ptrace and seccomp to be traced by the current process.
// launch_traced will return the PID of the newly created process, which should be running (or at
// least ready to be waited on) upon return.
shared_ptr<Process> Tracer::launchTraced(shared_ptr<Command> cmd) noexcept {
  LOG(exec) << "Executing " << cmd;

  // Get a reference to the directory where the command will be started
  auto cwd = cmd->getInitialWorkingDir();

  // Get a reference to the root directory in effect when the command is started
  auto root = cmd->getInitialRootDir();

  // Fill this vector in with {parent_fd, child_fd} pairs
  // The launched child will dup2 these into place
  vector<pair<int, int>> initial_fds;

  // Loop over the initial fds for the command we are launching
  for (const auto& [child_fd, info] : cmd->getInitialFDs()) {
    // Make sure the reference has already been resolved
    ASSERT(info.getRef()->isResolved())
        << "Tried to launch a command with an unresolved reference in its "
           "initial file descriptor table";

    // Get a file descriptor for the reference in the command's initial descriptor table, and record
    // how it should be re-numbered in the child
    initial_fds.emplace_back(info.getRef()->getFD(), child_fd);
  }

  // Launch a child process
  pid_t child_pid = fork();
  FAIL_IF(child_pid == -1) << "Failed to fork: " << ERR;

  if (child_pid == 0) {
    // This is the child

    // Set up FDs as requested. We assume that all parent FDs are marked CLOEXEC if
    // necessary and that there are no ordering constraints on duping (e.g. if the
    // child fd for one entry matches the parent fd of another).
    for (const auto& [parent_fd, child_fd] : initial_fds) {
      if (parent_fd != child_fd) {
        LOG(exec) << "Duping fd " << parent_fd << " to " << child_fd << " in child";
        int rc = dup2(parent_fd, child_fd);

        FAIL_IF(rc != child_fd) << "Failed to initialize fds: " << ERR;
      }
    }

    // Change to the initial working directory
    auto cwd = cmd->getInitialWorkingDir()->getArtifact();
    auto cwd_path = cwd->getPath(false);
    ASSERT(cwd_path.has_value()) << "Current working directory does not have a committed path";
    int rc = ::chdir(cwd_path.value().c_str());
    ASSERT(rc == 0) << "Failed to change to starting directory to launch " << cmd;

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
    auto exe = cmd->getExecutable()->getArtifact();
    auto exe_path = exe->getPath(false);
    ASSERT(exe_path.has_value()) << "Executable has no committed path";
    execv(exe_path.value().c_str(), (char* const*)args.data());

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

  // Resume and handle some number of seccomp stops
  do {
    FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;
    waitpid(child_pid, &wstatus, 0);
  } while (WIFSTOPPED(wstatus) && (wstatus >> 8) == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8)));

  // Make sure we left the loop on an exec event
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_EXEC << 8)))
      << "Unexpected stop from child. Expected EXEC";

  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  auto proc = make_shared<Process>(_build, *this, cmd, child_pid, cwd, root, cmd->getInitialFDs());
  _threads[child_pid] = make_shared<Thread>(_build, *this, proc, child_pid);

  return proc;
}
