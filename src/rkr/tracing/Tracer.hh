#pragma once

#include <list>
#include <memory>
#include <optional>
#include <tuple>
#include <unordered_map>

#include <sys/types.h>

#include "tracing/Thread.hh"

class Build;
class Command;
class Process;

class Tracer {
  friend class Process;

 public:
  /// Create a tracer linked to a specific rebuild environment
  Tracer(Build& build) noexcept : _build(build) {}

  // Disallow copy
  Tracer(const Tracer&) = delete;
  Tracer& operator=(const Tracer&) = delete;

  /// Start a command in this tracer
  std::shared_ptr<Process> start(const std::shared_ptr<Command>& cmd) noexcept;

  /// Wait for a specific process to exit, or all processes if unspecified
  void wait(std::shared_ptr<Process> p = nullptr) noexcept;

  /// Claim a process from the set of exited processes
  std::shared_ptr<Process> getExited(pid_t pid) noexcept;

 private:
  /// Get the next available traced event
  std::optional<std::tuple<pid_t, int>> getEvent() noexcept;

  /// Launch a command with tracing enabled
  std::shared_ptr<Process> launchTraced(const std::shared_ptr<Command>& cmd) noexcept;

  /// Called when we catch a system call in the traced process
  void handleSyscall(Thread& t) noexcept;

  /// Called after a traced process issues a clone system call
  void handleClone(Thread& t, int flags) noexcept;

  /// Called after a traced process issues a fork system call
  void handleFork(Thread& t) noexcept;

  /// Called when a traced process exits
  void handleExit(Thread& t, int exit_status) noexcept;

  /// Called when a traced process is killed by a signal
  void handleKilled(Thread& t, int exit_status, int term_sig) noexcept;

 public:
  inline static std::map<std::string, size_t> syscall_counts;
  inline static size_t ptrace_syscall_count = 0;
  inline static size_t fast_syscall_count = 0;

  static void printSyscallStats() noexcept;

 private:
  /// This tracer is executing commands on behalf of this build
  Build& _build;

  /// A map from thread IDs to threads
  std::unordered_map<pid_t, Thread> _threads;

  /// The map of processes that have exited
  std::unordered_map<pid_t, std::shared_ptr<Process>> _exited;

  /// Some tracing events appear before we can process them (e.g. in a child process before we've
  /// seen its creation. Store them here.
  std::list<std::tuple<pid_t, int>> _event_queue;
};
