#pragma once

#include <list>
#include <map>
#include <memory>
#include <optional>
#include <tuple>

#include <sys/types.h>

#include "util/log.hh"

using std::list;
using std::map;
using std::optional;
using std::shared_ptr;
using std::tuple;

class Build;
class Command;
class Process;

class Tracer {
  friend class Process;

 public:
  /// Create a tracer linked to a specific rebuild environment
  Tracer(Build& build) noexcept;

  /// Destroy this tracer
  ~Tracer() noexcept;

  /// Start a command in this tracer
  shared_ptr<Process> start(shared_ptr<Command> cmd) noexcept;

  /// Wait for a specific process to exit, or all processes if unspecified
  void wait(shared_ptr<Process> p = nullptr) noexcept;

  /// Try to clean up any remaining processes managed by this tracer
  void cleanup() noexcept;

 private:
  /// Get the next available traced event
  optional<tuple<pid_t, int>> getEvent(bool block = true) noexcept;

  /// Launch a command with tracing enabled
  shared_ptr<Process> launchTraced(shared_ptr<Command> cmd) noexcept;

  /// Called when we catch a system call in the traced process
  void handleSyscall(shared_ptr<Process> p) noexcept;

  /// Called after a traced process issues a clone system call
  void handleClone(shared_ptr<Process> p, int flags) noexcept;

  /// Called after a traced process issues a fork system call
  void handleFork(shared_ptr<Process> p) noexcept;

  /// Called when a traced process exits
  void handleExit(shared_ptr<Process> p) noexcept;

  /// Claim a process from the set of exited processes
  shared_ptr<Process> getExited(pid_t pid) noexcept;

 private:
  /// This tracer is executing commands on behalf of this build
  Build& _build;

  /// A map from process IDs to processes. Note that a process will appear multiple times if it uses
  /// multiple threads; all entries will point to the same Process instance.
  map<pid_t, shared_ptr<Process>> _processes;

  /// The map of processes that have exited
  map<pid_t, shared_ptr<Process>> _exited;

  /// Some tracing events appear before we can process them (e.g. in a child process before we've
  /// seen its creation. Store them here.
  list<tuple<pid_t, int>> _event_queue;
};
