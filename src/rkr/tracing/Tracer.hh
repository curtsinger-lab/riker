#pragma once

#include <list>
#include <memory>
#include <optional>
#include <tuple>
#include <unordered_map>

#include <sys/types.h>

#include "tracing/Thread.hh"
#include "tracing/inject.h"

class Build;
class Command;
class Process;

class Tracer {
  friend class Process;

 public:
  /// Create a tracer linked to a specific rebuild environment
  Tracer() noexcept {}

  // Disallow copy
  Tracer(const Tracer&) = delete;
  Tracer& operator=(const Tracer&) = delete;

  /// Start a command in this tracer
  std::shared_ptr<Process> start(Build& build, const std::shared_ptr<Command>& cmd) noexcept;

  /// Wait for a specific process to exit, or all processes if unspecified
  void wait(Build& build, std::shared_ptr<Process> p = nullptr) noexcept;

  /// Claim a process from the set of exited processes
  std::shared_ptr<Process> getExited(pid_t pid) noexcept;

 private:
  /// Get the next available traced event
  std::optional<std::tuple<pid_t, int>> getEvent() noexcept;

  /// Launch a command with tracing enabled
  std::shared_ptr<Process> launchTraced(Build& build, const std::shared_ptr<Command>& cmd) noexcept;

  /// Called when we catch a system call in the traced process
  void handleSyscall(Thread& t) noexcept;

  /// Called after a traced process issues a clone system call
  void handleClone(Build& build, Thread& t, int flags) noexcept;

  /// Called after a traced process issues a fork system call
  void handleFork(Build& build, Thread& t) noexcept;

  /// Called when a traced process exits
  void handleExit(Thread& t, int exit_status) noexcept;

  /// Called when a traced process is killed by a signal
  void handleKilled(Build& build, Thread& t, int exit_status, int term_sig) noexcept;

 public:
  inline static std::map<std::string, size_t> syscall_counts;
  inline static size_t ptrace_syscall_count = 0;
  inline static size_t fast_syscall_count = 0;

  static void printSyscallStats() noexcept;

  /// Get the system call being traced through the specified shared memory channel
  static long getSyscallNumber(ssize_t channel) noexcept;

  /// Get the register state for a specified shared memory channel
  static const user_regs_struct& getRegisters(ssize_t channel) noexcept;

  /// Get the result of the system call being traced through a shared memory channel
  static long getSyscallResult(ssize_t channel) noexcept;

  /// Allow a tracee blocked on a shared memory channel to proceed
  static void channelProceed(ssize_t channel, bool stop_on_exit) noexcept;

  /// Force a tracee to exit instead of running a system call
  static void channelForceExit(ssize_t channel, int exit_status) noexcept;

  /// Get the data buffer associated with a shared memory channel
  static void* channelGetBuffer(ssize_t channel) noexcept;

 private:
  /// A map from thread IDs to threads
  std::unordered_map<pid_t, Thread> _threads;

  /// The map of processes that have exited
  std::unordered_map<pid_t, std::shared_ptr<Process>> _exited;

  /// Some tracing events appear before we can process them (e.g. in a child process before we've
  /// seen its creation. Store them here.
  std::list<std::tuple<pid_t, int>> _event_queue;

  /// The file descriptor for the shared memory tracing channels
  inline static int _trace_data_fd = -1;

  /// A pointer to the shared memory tracing data
  inline static struct shared_tracing_data* _shmem = nullptr;
};
