#pragma once

#include <map>
#include <memory>

#include <sys/types.h>

#include "util/log.hh"

using std::map;
using std::shared_ptr;

class Build;
class Command;
class Process;

class Tracer {
 public:
  /// Create a tracer linked to a specific rebuild environment
  Tracer(Build& build) noexcept;

  /// Destroy thsi tracer
  ~Tracer() noexcept;

  /// Run a command in this tracer
  void run(shared_ptr<Command> cmd) noexcept;

  /// Try to clean up any remaining processes managed by this tracer
  void cleanup() noexcept;

 private:
  /// Launch a command with tracing enabled
  void launchTraced(shared_ptr<Command> cmd) noexcept;

  /// Called when we catch a system call in the traced process
  void handleSyscall(shared_ptr<Process> p) noexcept;

  /// Called after a traced process issues a clone system call
  void handleClone(shared_ptr<Process> p, int flags) noexcept;

  /// Called after a traced process issues a fork system call
  void handleFork(shared_ptr<Process> p) noexcept;

  /// Called when a traced process exits
  void handleExit(shared_ptr<Process> p) noexcept;

 private:
  /// This tracer is executing commands on behalf of this build
  Build& _build;

  /// A map from process IDs to processes. Note that a process will appear multiple times if it uses
  /// multiple threads; all entries will point to the same Process instance.
  map<pid_t, shared_ptr<Process>> _processes;
};
