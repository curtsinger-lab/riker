#include "Tracer.hh"

#include <cerrno>
#include <csignal>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include <emmintrin.h>
#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <semaphore.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <unistd.h>

#include "artifacts/Artifact.hh"
#include "runtime/Build.hh"
#include "runtime/Command.hh"
#include "runtime/Ref.hh"
#include "tracing/Process.hh"
#include "tracing/SyscallTable.hh"
#include "tracing/Thread.hh"
#include "tracing/inject.h"
#include "util/log.hh"
#include "util/stats.hh"
#include "util/wrappers.hh"
#include "versions/FileVersion.hh"

using std::ifstream;
using std::list;
using std::make_shared;
using std::map;
using std::nullopt;
using std::optional;
using std::set;
using std::shared_ptr;
using std::string;
using std::tuple;
using std::vector;

namespace fs = std::filesystem;

// The BPF program (initialized on first use)
vector<struct sock_filter> bpf;

// Stub for the seccomp syscall
int seccomp(unsigned int operation, unsigned int flags, void* args) {
  return syscall(__NR_seccomp, operation, flags, args);
}

shared_ptr<Process> Tracer::start(Build& build, const shared_ptr<Command>& cmd) noexcept {
  // Launch the command with tracing
  return launchTraced(build, cmd);
}

optional<tuple<pid_t, int>> Tracer::getEvent(Build& build) noexcept {
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

  size_t spin_count = 0;

  // Wait for an event from ptrace
  while (true) {
    // Check the shared memory channel
    if (_shmem != nullptr) {
      // Loop over all the shared memory channels
      for (size_t i = 0; i < TRACING_CHANNEL_COUNT; i++) {
        auto state = __atomic_load_n(&_shmem->channels[i].state, __ATOMIC_ACQUIRE);

        if (state == CHANNEL_STATE_PRE_SYSCALL_WAIT || state == CHANNEL_STATE_POST_SYSCALL_NOTIFY ||
            state == CHANNEL_STATE_POST_SYSCALL_WAIT) {
          // Reset the state so we don't try to handle this event again later
          _shmem->channels[i].state = CHANNEL_STATE_OBSERVED;

          // Find the thread using this channel
          auto iter = _threads.find(_shmem->channels[i].tid);
          if (iter != _threads.end()) {
            if (state == CHANNEL_STATE_PRE_SYSCALL_WAIT) {
              iter->second.syscallEntryChannel(build, i);
            } else if (state == CHANNEL_STATE_POST_SYSCALL_NOTIFY) {
              FAIL << "Channel is in post-syscall notify state, which is not yet handled";
            } else if (state == CHANNEL_STATE_POST_SYSCALL_WAIT) {
              iter->second.syscallExitChannel(build, i);
            }
          } else {
            WARN << "Tracing channel is owned by unrecognized thread " << _shmem->channels[i].tid;
          }
        }
      }
    }

    // Check for a child, but do not block
    int wait_status;
    pid_t child = ::waitpid(-1, &wait_status, WNOHANG);

    // Did waitpid return an error?
    if (child == -1) {
      // If errno is ECHILD, we're done and can return with no event
      if (errno == ECHILD)
        return nullopt;
      else
        FAIL << "Error while waiting: " << ERR;

    } else if (child > 0) {
      // A child responded to waitpid. Handle its event now

      // Count the ptrace stop for this event
      stats::ptrace_stops++;

      // Does this event refer to a process we don't know about yet?
      if (_threads.find(child) == _threads.end()) {
        // Yes. Queue the event so we can try another one.
        _event_queue.emplace_back(child, wait_status);
      } else {
        // No. The event is for a known process. Return it now.
        return tuple{child, wait_status};
      }
    } else {
      // If there were no ptrace events, just pause briefly
      if (++spin_count % 16 == 0) {
        sched_yield();
      } else {
        _mm_pause();
      }
    }
  }
}

void Tracer::wait(Build& build, shared_ptr<Process> p) noexcept {
  if (p) {
    LOG(exec) << "Waiting for " << p;
  } else {
    LOG(exec) << "Waiting for all remaining processes";
  }

  // Process tracaing events
  while (true) {
    // If we're waiting for a specific process, and that process has exited, return now
    if (p && p->hasExited()) return;

    auto e = getEvent(build);
    if (!e.has_value()) return;

    auto [child, wait_status] = e.value();

    auto& thread = _threads.at(child);

    if (WIFSTOPPED(wait_status)) {
      int status = wait_status >> 8;

      if (status == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
        // Stopped on entry to a syscall
        handleSyscall(build, thread);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_FORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_VFORK << 8)) ||
                 status == (SIGTRAP | (PTRACE_EVENT_FORK << 8) | (PTRACE_EVENT_VFORK << 8))) {
        handleFork(build, thread);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_CLONE << 8))) {
        auto regs = thread.getRegisters();
        handleClone(build, thread, regs.SYSCALL_ARG1);

      } else if (status == (SIGTRAP | 0x80)) {
        // This is a stop at the end of a system call that was resumed.
        thread.syscallExitPtrace(build);

      } else if (status == (SIGTRAP | (PTRACE_EVENT_EXEC << 8))) {
        // This is a stop after an exec finishes.
        thread.execPtrace(build);

      } else if (status == (PTRACE_EVENT_STOP << 8)) {
        // Is this delivering a stopping signal?
        if (WSTOPSIG(wait_status) == SIGSTOP || WSTOPSIG(wait_status) == SIGTSTP ||
            WSTOPSIG(wait_status) == SIGTTIN || WSTOPSIG(wait_status) == SIGTTOU) {
          // Yes. The tracee is in group-stop state
          WARN << thread << " in group-stop with signal " << getSignalName(WSTOPSIG(wait_status));
          FAIL_IF(ptrace(PTRACE_LISTEN, child, nullptr, 0))
              << "Failed to put tracee in listen state after group-stop: " << ERR;

        } else {
          // No. Just resume the child without delivering a signal
          FAIL_IF(ptrace(PTRACE_CONT, child, nullptr, 0))
              << "Failed to resume child after PTRACE_EVENT_STOP: " << ERR;
        }

      } else {
        // The traced process received a signal. Just pass it along.
        LOG(trace) << thread << ": injecting signal " << getSignalName(WSTOPSIG(wait_status))
                   << " (status=" << status << ")";
        ptrace(PTRACE_CONT, child, nullptr, WSTOPSIG(wait_status));
      }

    } else if (WIFEXITED(wait_status)) {
      // Stopped on exit
      handleExit(build, thread, WEXITSTATUS(wait_status));

    } else if (WIFSIGNALED(wait_status)) {
      // Killed by a signal
      handleKilled(build, thread, WEXITSTATUS(wait_status), WTERMSIG(wait_status));
    }
  }
}

void Tracer::handleClone(Build& build, Thread& t, int flags) noexcept {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new thread id and then resume execution
  pid_t new_tid = t.getEventMessage();
  t.resume();

  // TODO: Handle flags

  // Threads in the same process just appear as pid references to the same process
  _threads.emplace(new_tid, Thread(*this, t.getProcess(), new_tid));
}

void Tracer::handleFork(Build& build, Thread& t) noexcept {
  // NOTE: This is not truly a syscall trap. Instead, it's a ptrace event. This handler runs after
  // the syscall has done most of the work

  // Get the new process id and resume execution
  pid_t new_pid = t.getEventMessage();
  t.resume();

  // If the call failed, do nothing
  if (new_pid == -1) return;

  // Create a new process running the same command
  auto new_proc = t.getProcess()->fork(build, new_pid);

  // Record a new thread running in this process. It is the main thread, so pid and tid will be
  // equal
  _threads.emplace(new_pid, Thread(*this, new_proc, new_pid));
}

void Tracer::handleExit(Build& build, Thread& t, int exit_status) noexcept {
  LOGF(trace, "{}: exited", t);

  // Is the thread that's exiting the main thread in its process?
  auto proc = t.getProcess();
  if (t.getID() == proc->getID()) {
    LOGF(trace, "{}: exited", proc);
    proc->exit(build, exit_status);
    _exited.emplace(proc->getID(), proc);
  }

  _threads.erase(t.getID());
}

void Tracer::handleKilled(Build& build, Thread& t, int exit_status, int term_sig) noexcept {
  // Keep a set of signals that cause a program to dump core
  static set<int> core_signals = {SIGABRT, SIGBUS,  SIGCONT, SIGFPE,  SIGILL,  SIGIOT,
                                  SIGQUIT, SIGSEGV, SIGSYS,  SIGTRAP, SIGXCPU, SIGXFSZ};

  // Was the thread killed by a signal that will dump core?
  if (core_signals.find(term_sig) != core_signals.end()) {
    // Yes. A core file may exist. If it does exist, we need to add trace events to attribute that
    // creation to the command that is dying via signal now.

    // Get a reference to the working directory, which is where a core file will be created
    auto cwd_ref_id = t.getProcess()->getWorkingDir();
    const auto& cwd_ref = t.getCommand()->getRef(cwd_ref_id);
    ASSERT(cwd_ref->isResolved()) << t.getProcess() << " is running with an unresolved working dir";

    // Try to get a path to the working directory
    const auto& cwd = cwd_ref->getArtifact();
    auto cwd_path = cwd->getCommittedPath();

    // We only have to do work if the working directory has a path
    if (cwd_path.has_value()) {
      // Check for a core file
      auto core_path = cwd_path.value() / "core";
      struct stat statbuf;
      if (::stat(core_path.c_str(), &statbuf) == 0) {
        // Make a reference to the core file that creates it
        auto core_ref = t.getCommand()->nextRef();
        build.pathRef(t.getCommand(), cwd_ref_id, "core", AccessFlags{.w = true, .create = true},
                      core_ref);
        auto core = t.getCommand()->getRef(core_ref)->getArtifact();

        // Make sure the reference resolved
        if (core) {
          // Create a version to represent the core file
          auto cv = make_shared<FileVersion>(statbuf);

          // Trace a write to the core file from the command that's exiting
          build.updateContent(t.getCommand(), core_ref, cv);

        } else {
          WARN << "Model did not allow for creation of a core file at " << core_path;
        }
      }
    }
  }

  // Finish handling the exit
  handleExit(build, t, exit_status);
}

shared_ptr<Process> Tracer::getExited(pid_t pid) noexcept {
  auto result = _exited[pid];
  _exited.erase(pid);
  return result;
}

string findLibraryOffset(pid_t pid, uintptr_t ptr) {
  ifstream maps("/proc/" + std::to_string(pid) + "/maps");

  while (maps.good() && !maps.eof()) {
    uintptr_t base, limit;
    char perms[5];
    size_t offset;
    size_t dev_major, dev_minor;
    uintptr_t inode;
    string path;

    // Skip over whitespace
    maps >> std::skipws;

    // Read in "<base>-<limit> <perms> <offset> <dev_major>:<dev_minor> <inode>"
    maps >> std::hex >> base;
    if (maps.get() != '-') break;
    maps >> std::hex >> limit;

    if (maps.get() != ' ') break;
    maps.get(perms, 5);

    maps >> std::hex >> offset;
    maps >> std::hex >> dev_major;
    if (maps.get() != ':') break;
    maps >> std::hex >> dev_minor;
    maps >> std::dec >> inode;

    // Skip over spaces and tabs
    while (maps.peek() == ' ' || maps.peek() == '\t') {
      maps.ignore(1);
    }

    // Read out the mapped file's path
    getline(maps, path);

    // Does the queried pointer fall in bounds?
    if (!path.empty() && ptr >= base && ptr < limit) {
      std::stringstream ss;
      ss << path << " + " << std::hex << (ptr - base + offset);
      return ss.str();
    }
  }

  return "unknown";
}

void Tracer::handleSyscall(Build& build, Thread& t) noexcept {
  auto regs = t.getRegisters();

  const auto& entry = SyscallTable<Build>::get(regs.SYSCALL_NUMBER);

  // WARN << entry.getName() << " call at " << (void*)regs.INSTRUCTION_POINTER << " in " <<
  // t.getCommand();

  if (entry.isTraced()) {
    LOG(trace) << t << ": stopped on syscall " << entry.getName();

    if (options::syscall_stats) {
      std::stringstream ss;
      ss << entry.getName() << " (ptrace "
         << findLibraryOffset(t.getProcess()->getID(), regs.INSTRUCTION_POINTER) << ")";
      Tracer::syscall_counts[ss.str()]++;
      Tracer::ptrace_syscall_count++;
    }

    // Run the system call handler
    entry.runHandler(build, t, regs);

  } else {
    FAIL << "Traced system call number " << regs.SYSCALL_NUMBER << " in " << t;
  }
}

// Launch a program fully set up with ptrace and seccomp to be traced by the current process.
// launch_traced will return the PID of the newly created process, which should be running (or at
// least ready to be waited on) upon return.
shared_ptr<Process> Tracer::launchTraced(Build& build, const shared_ptr<Command>& cmd) noexcept {
  LOG(exec) << "Preparing to trace " << cmd;

  // First mark all FDs as close-on-exec
  for (auto& entry : fs::directory_iterator("/proc/self/fd")) {
    int fd = std::stoi(entry.path().filename());

    // Skip the shared memory channel fd
    if (fd == TRACING_CHANNEL_FD) continue;

    int flags = fcntl(fd, F_GETFD, 0);
    WARN_IF(flags < 0) << "Failed to get flags for fd " << fd;

    // If the flags do not include the cloexec bit, turn it on
    if ((flags & FD_CLOEXEC) == 0) {
      flags |= FD_CLOEXEC;
      int rc = fcntl(fd, F_SETFD, flags);
      WARN_IF(rc < 0) << "Failed to set flags for fd " << fd;
    }
  }

  // Fill this vector in with {parent_fd, child_fd} pairs
  // The launched child will dup2 these into place
  vector<std::pair<int, int>> initial_fds;

  // Loop over the initial fds for the command we are launching
  for (const auto& [child_fd, ref_id] : cmd->getInitialFDs()) {
    auto& ref = cmd->getRef(ref_id);

    LOG(exec) << "  Setting up fd " << child_fd << " with reference " << ref;

    // Make sure the reference has already been resolved
    ASSERT(ref->isResolved()) << "Tried to launch a command with an unresolved reference in its "
                                 "initial file descriptor table";

    // Get a file descriptor for the reference in the command's initial descriptor table, and
    // record how it should be re-numbered in the child
    initial_fds.emplace_back(ref->getFD(), child_fd);
  }

  // Is the trace channel temporary file not yet initialized?
  if (options::inject_tracing_lib && _trace_data_fd == -1) {
    // Set up the trace channel fd now
    int fd = open("/tmp/", O_RDWR | O_TMPFILE, 0600);
    FAIL_IF(fd < 0) << "Failed to create temporary file for shared tracing channel.";

    // Extend the channel to the requested size
    FAIL_IF(ftruncate(fd, sizeof(struct shared_tracing_data)))
        << "Failed to extend shared tracing channel to requested size.";

    // Now dup the file descriptor to the expected number
    FAIL_IF(dup2(fd, TRACING_CHANNEL_FD) != TRACING_CHANNEL_FD)
        << "Failed to shift tracing channel fd to known index: " << ERR;

    // Close the original fd
    close(fd);

    // Save the fd
    _trace_data_fd = TRACING_CHANNEL_FD;

    // Try to mmap the channel
    void* p = mmap(NULL, sizeof(struct shared_tracing_data), PROT_READ | PROT_WRITE, MAP_SHARED,
                   _trace_data_fd, 0);
    if (p == MAP_FAILED) {
      WARN << "Failed to mmap shared memory channel in tracer: " << ERR;

      // Close the trace channel fd so the tracee doesn't use it
      close(_trace_data_fd);

    } else {
      // Set the shared channel global pointer
      _shmem = (struct shared_tracing_data*)p;

      // Zero out the tracing channel data
      memset(_shmem, 0, sizeof(struct shared_tracing_data));

      // Initialize the semaphore that tracees use to coordinate channel acquisition
      sem_init(&_shmem->available, 1, TRACING_CHANNEL_COUNT);

      // Initialize the semaphores used to wake tracees in each channel
      for (size_t i = 0; i < TRACING_CHANNEL_COUNT; i++) {
        sem_init(&_shmem->channels[i].wake_tracee, 1, 0);
      }
    }
  }

  // If the bpf program hasn't been generated yet, do that now
  if (bpf.size() == 0) {
    // Compute the offset of the instruction pointer in the seccomp_data struct
    uint32_t ip_offset = offsetof(struct seccomp_data, instruction_pointer);

    // Load the lower four bytes of the instruction pointer
    bpf.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, ip_offset));

    uint32_t safe_page_lower = ((intptr_t)SAFE_SYSCALL_PAGE) & 0xFFFFFFFF;
    uint32_t safe_page_upper = ((intptr_t)SAFE_SYSCALL_PAGE) >> 32;

    // If the lower four bytes are less than the safe syscall page, jump ahead
    bpf.push_back(BPF_JUMP(BPF_JMP | BPF_JGE | BPF_K, safe_page_lower, 0, 4));

    // If the lower four bytes are greater than or equal to 0x77771000, jump ahead
    bpf.push_back(BPF_JUMP(BPF_JMP | BPF_JGE | BPF_K, safe_page_lower + 0x1000, 3, 0));

    // Load the upper four bytes of the instruction pointer
    bpf.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, ip_offset + 4));

    // If the upper four bytes are not zero, jump ahead
    bpf.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, safe_page_upper, 0, 1));

    // If we hit this point, this is an allowed syscall
    bpf.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW));

    // Load the syscall number
    bpf.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr)));

    // Loop over syscalls
    for (uint32_t i = 0; i < SyscallTable<Build>::size(); i++) {
      // Is this the mmap syscall entry?
      if (i == __NR_mmap) {
        bpf.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, i, 0, 4));

        // Load the fd argument
        bpf.push_back(BPF_STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, args[4])));

        // If fd is -1, allow the syscall. Otherwise trace it.
        bpf.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, static_cast<uint32_t>(-1), 0, 1));
        bpf.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW));
        bpf.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_TRACE));

      } else {
        if (SyscallTable<Build>::get(i).isTraced()) {
          // Check if the syscall matches the current entry. If it matches, trace the syscall.
          bpf.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, i, 0, 1));
          bpf.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_TRACE));

        } else {
          // Check if the syscall matches the current entry. If it does, allow the syscall.
          bpf.push_back(BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, i, 0, 1));
          bpf.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW));
        }
      }
    }

    // Default case allows the syscall
    bpf.push_back(BPF_STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW));
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
        int rc = dup2(parent_fd, child_fd);

        FAIL_IF(rc != child_fd) << "Failed to initialize fds: " << ERR;
      } else {
        int flags = fcntl(parent_fd, F_GETFD, 0);
        FAIL_IF(flags < 0) << "Failed to get flags for fd " << parent_fd;

        flags &= ~FD_CLOEXEC;
        int rc = fcntl(parent_fd, F_SETFD, flags);
        FAIL_IF(rc < 0) << "Failed to set flags for fd " << parent_fd;
      }
    }

    // Change to the initial working directory
    auto cwd = cmd->getRef(Ref::Cwd)->getArtifact();
    auto cwd_path = cwd->getCommittedPath();
    ASSERT(cwd_path.has_value()) << "Current working directory does not have a committed path";
    int rc = ::chdir(cwd_path.value().c_str());
    FAIL_IF(rc != 0) << "Failed to chdir to " << cwd_path.value() << " to launch " << cmd;

    // TODO: Change to the appropriate root directory

    // Lock down the process so that we are allowed to
    // use seccomp without special permissions
    FAIL_IF(prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0) << "Failed to allow seccomp: " << ERR;

    struct sock_fprog bpf_program;
    bpf_program.filter = bpf.data();
    bpf_program.len = bpf.size();

    // Actually enable the filter
    FAIL_IF(seccomp(SECCOMP_SET_MODE_FILTER, SECCOMP_FILTER_FLAG_SPEC_ALLOW, &bpf_program) != 0)
        << "Error enabling seccomp: " << ERR;

    // Raise SIGSTOP so the parent can resume this process once ptrace is all set up
    // raise(SIGSTOP);

    vector<const char*> args;
    for (const auto& s : cmd->getArguments()) {
      args.push_back(s.c_str());
    }

    // Null-terminate the args array
    args.push_back(nullptr);

    // Add the injected library to the environment
    if (options::inject_tracing_lib) {
      std::string ld_preload =
          (readlink("/proc/self/exe").parent_path() / "../share/rkr/rkr-inject.so").string();
      if (char* old_ld_preload = getenv("LD_PRELOAD"); old_ld_preload != NULL) {
        ld_preload += ":" + std::string(old_ld_preload);
      }
      setenv("LD_PRELOAD", ld_preload.c_str(), 1);
    }

    if (options::parallel_wrapper) {
      std::string path =
          readlink("/proc/self/exe").parent_path().string() + "/../share/rkr/wrappers";
      if (char* old_path = getenv("PATH"); old_path != NULL) {
        path += ":" + std::string(old_path);
      }
      setenv("PATH", path.c_str(), 1);
    }

    // TODO: explicitly handle the environment
    auto exe = cmd->getRef(Ref::Exe)->getArtifact();
    auto exe_path = exe->getCommittedPath();
    ASSERT(exe_path.has_value()) << "Executable has no committed path";
    execv(exe_path.value().c_str(), (char* const*)args.data());

    // This is unreachable, unless execv fails
    FAIL << "Failed to start traced program: " << ERR;
  }

  // Set up options to handle everything reliably. We do this before continuing
  // so that the actual running program has everything properly configured.
  int options = 0;
  options |= PTRACE_O_TRACEFORK | PTRACE_O_TRACECLONE | PTRACE_O_TRACEVFORK;  // Follow forks
  options |= PTRACE_O_TRACEEXEC;     // Handle execs more reliably
  options |= PTRACE_O_TRACESYSGOOD;  // When stepping through syscalls, be clear
  options |= PTRACE_O_TRACESECCOMP;  // Actually receive the syscall stops we requested
  options |= PTRACE_O_EXITKILL;      // Kill tracees on exit

  FAIL_IF(ptrace(PTRACE_SEIZE, child_pid, nullptr, options))
      << "Failed to seize child pid: " << ERR;

  // The tracee will stop a few times as it issues system calls captured via seccomp. Ignore
  // these.
  int wstatus;
  waitpid(child_pid, &wstatus, 0);  // Should correspond to raise(SIGSTOP)
  while (WIFSTOPPED(wstatus) && (wstatus >> 8) == (SIGTRAP | (PTRACE_EVENT_SECCOMP << 8))) {
    FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;
    waitpid(child_pid, &wstatus, 0);
  }

  // Make sure we left the loop on an exec event
  FAIL_IF(!WIFSTOPPED(wstatus) || (wstatus >> 8) != (SIGTRAP | (PTRACE_EVENT_EXEC << 8)))
      << "Unexpected stop from child. Expected EXEC";

  // Now the tracee can run the launched command
  FAIL_IF(ptrace(PTRACE_CONT, child_pid, nullptr, 0)) << "Failed to resume child: " << ERR;

  map<int, Process::FileDescriptor> fds;
  for (auto& [fd, ref] : cmd->getInitialFDs()) {
    fds[fd] = Process::FileDescriptor{ref, false};
  }

  auto proc = make_shared<Process>(build, cmd, child_pid, Ref::Cwd, Ref::Root, fds);
  _threads.emplace(child_pid, Thread(*this, proc, child_pid));

  // The process is the primary process for its command
  proc->setPrimary();

  return proc;
}

void Tracer::printSyscallStats() noexcept {
  vector<std::pair<std::string, size_t>> sorted(Tracer::syscall_counts.begin(),
                                                Tracer::syscall_counts.end());
  std::sort(sorted.begin(), sorted.end(),
            [](const auto& a, const auto& b) { return a.second > b.second; });

  std::cout << "System Call Stats:" << std::endl;
  for (const auto& [name, count] : sorted) {
    if (count > 100) std::cout << "  " << name << ": " << count << std::endl;
  }

  std::cout << std::endl;

  size_t total_syscalls = Tracer::fast_syscall_count + Tracer::ptrace_syscall_count;
  size_t percent_fast = (100 * Tracer::fast_syscall_count) / total_syscalls;
  std::cout << Tracer::fast_syscall_count << "/" << total_syscalls << " (" << percent_fast
            << "%) syscalls handed by fast tracing" << std::endl;
}

// Get the system call being traced through the specified shared memory channel
long Tracer::getSyscallNumber(ssize_t i) noexcept {
  return _shmem->channels[i].regs.SYSCALL_NUMBER;
}

// Get the register state for a specified shared memory channel
const user_regs_struct& Tracer::getRegisters(ssize_t i) noexcept {
  return _shmem->channels[i].regs;
}

// Get the result of the system call being traced through a shared memory channel
long Tracer::getSyscallResult(ssize_t i) noexcept {
  return _shmem->channels[i].regs.SYSCALL_RETURN;
}

// Let the tracee resume without blocking again
void Tracer::channelContinue(ssize_t i) noexcept {
  ASSERT(_shmem->channels[i].state == CHANNEL_STATE_OBSERVED) << "Channel is not blocked";
  _shmem->channels[i].action = CHANNEL_ACTION_CONTINUE;
  __atomic_store_n(&_shmem->channels[i].state, CHANNEL_STATE_PROCEED, __ATOMIC_RELEASE);
  while (sem_post(&_shmem->channels[i].wake_tracee) == -1) {
    WARN_IF(errno != EAGAIN) << "Error from sem_post: " << ERR;
  }
}

// Ask the tracee to finish the system call and report the result without blocking
void Tracer::channelNotify(ssize_t i) noexcept {
  ASSERT(_shmem->channels[i].state == CHANNEL_STATE_OBSERVED) << "Channel is not blocked";
  _shmem->channels[i].action = CHANNEL_ACTION_NOTIFY;
  __atomic_store_n(&_shmem->channels[i].state, CHANNEL_STATE_PROCEED, __ATOMIC_RELEASE);
  while (sem_post(&_shmem->channels[i].wake_tracee) == -1) {
    WARN_IF(errno != EAGAIN) << "Error from sem_post: " << ERR;
  }
}

// Ask the tracee to finish the system call and block again
void Tracer::channelFinish(ssize_t i) noexcept {
  ASSERT(_shmem->channels[i].state == CHANNEL_STATE_OBSERVED) << "Channel is not blocked";
  _shmem->channels[i].action = CHANNEL_ACTION_FINISH;
  __atomic_store_n(&_shmem->channels[i].state, CHANNEL_STATE_PROCEED, __ATOMIC_RELEASE);
  while (sem_post(&_shmem->channels[i].wake_tracee) == -1) {
    WARN_IF(errno != EAGAIN) << "Error from sem_post: " << ERR;
  }
}

// Ask the tracee to exit instead of running the system call
void Tracer::channelExit(ssize_t i, int exit_status) noexcept {
  ASSERT(_shmem->channels[i].state == CHANNEL_STATE_OBSERVED) << "Channel is not blocked";
  _shmem->channels[i].action = CHANNEL_ACTION_EXIT;
  _shmem->channels[i].regs.SYSCALL_ARG1 = exit_status;
  __atomic_store_n(&_shmem->channels[i].state, CHANNEL_STATE_PROCEED, __ATOMIC_RELEASE);
  while (sem_post(&_shmem->channels[i].wake_tracee) == -1) {
    WARN_IF(errno != EAGAIN) << "Error from sem_post: " << ERR;
  }
}

// Ask the tracee to skip the system call and use the provided result instead
void Tracer::channelSkip(ssize_t i, long result) noexcept {
  ASSERT(_shmem->channels[i].state == CHANNEL_STATE_OBSERVED) << "Channel is not blocked";
  _shmem->channels[i].action = CHANNEL_ACTION_SKIP;
  _shmem->channels[i].regs.SYSCALL_RETURN = result;
  __atomic_store_n(&_shmem->channels[i].state, CHANNEL_STATE_PROCEED, __ATOMIC_RELEASE);
  while (sem_post(&_shmem->channels[i].wake_tracee) == -1) {
    WARN_IF(errno != EAGAIN) << "Error from sem_post: " << ERR;
  }
}

void* Tracer::channelGetBuffer(ssize_t i) noexcept {
  return _shmem->channels[i].buffer;
}
