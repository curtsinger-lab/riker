#pragma once

#include <list>
#include <memory>
#include <optional>
#include <tuple>
#include <unordered_map>

#include <sys/types.h>

#include "util/log.hh"

using std::list;
using std::optional;
using std::shared_ptr;
using std::tuple;
using std::unordered_map;

class Build;
class Command;
class Process;
class Thread;

class Tracer {
  friend class Process;

 public:
  /// Create a tracer linked to a specific rebuild environment
  Tracer(Build& build) noexcept : _build(build) {}

  // Disallow copy
  Tracer(const Tracer&) = delete;
  Tracer& operator=(const Tracer&) = delete;

  /// Start a command in this tracer
  shared_ptr<Process> start(const shared_ptr<Command>& cmd) noexcept;

  /// Wait for a specific process to exit, or all processes if unspecified
  void wait(shared_ptr<Process> p = nullptr) noexcept;

  /// Claim a process from the set of exited processes
  shared_ptr<Process> getExited(pid_t pid) noexcept;

 private:
  /// Get the next available traced event
  optional<tuple<pid_t, int>> getEvent(bool block = true) noexcept;

  /// Launch a command with tracing enabled
  shared_ptr<Process> launchTraced(const shared_ptr<Command>& cmd) noexcept;

  /// Called when we catch a system call in the traced process
  void handleSyscall(shared_ptr<Thread> t) noexcept;

  /// Called after a traced process issues a clone system call
  void handleClone(shared_ptr<Thread> t, int flags) noexcept;

  /// Called after a traced process issues a fork system call
  void handleFork(shared_ptr<Thread> t) noexcept;

  /// Called when a traced process exits
  void handleExit(shared_ptr<Thread> t) noexcept;

 private:
  /// This tracer is executing commands on behalf of this build
  Build& _build;

  /// A map from thread IDs to threads
  unordered_map<pid_t, shared_ptr<Thread>> _threads;

  /// The map of processes that have exited
  unordered_map<pid_t, shared_ptr<Process>> _exited;

  /// Some tracing events appear before we can process them (e.g. in a child process before we've
  /// seen its creation. Store them here.
  list<tuple<pid_t, int>> _event_queue;
};
