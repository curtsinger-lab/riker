#pragma once

#include <map>
#include <memory>

#include <sys/types.h>

using std::map;
using std::shared_ptr;

class Build;
class Command;
class Process;

class Tracer {
 public:
  /// Create a tracer linked to a specific rebuild environment
  Tracer(Build& build) : _build(build) {}

  /// Run a command in this tracer
  void run(const shared_ptr<Command>& cmd);

 private:
  /// Launch a command with tracing enabled
  void launchTraced(const shared_ptr<Command>& cmd);

  /// Called when we catch a system call in the traced process
  void handleSyscall(const shared_ptr<Process>& p);

  /// Called after a traced process issues a clone system call
  void handleClone(const shared_ptr<Process>& p, int flags);

  /// Called after a traced process issues a fork system call
  void handleFork(const shared_ptr<Process>& p);

  /// Called when a traced process exits
  void handleExit(const shared_ptr<Process>& p);

 private:
  /// This tracer is executing commands on behalf of this build
  Build& _build;

  /// A map from process IDs to processes. Note that a process will appear multiple times if it uses
  /// multiple threads; all entries will point to the same Process instance.
  map<pid_t, shared_ptr<Process>> _processes;
};
