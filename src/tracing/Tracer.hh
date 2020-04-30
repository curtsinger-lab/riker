#pragma once

#include <map>
#include <memory>

using std::map;
using std::shared_ptr;

class Process;
class Reference;
class Rebuild;
class Command;

class Tracer {
 public:
  /// Create a tracer linked to a specific rebuild environment
  Tracer(Rebuild& rebuild) : _rebuild(rebuild) {}

  /// Run a command in this tracer
  void run(shared_ptr<Command> cmd);

 private:
  /// Launch a command with tracing enabled
  void launchTraced(shared_ptr<Command> cmd);

  /// Called when we catch a system call in the traced process
  void handleSyscall(shared_ptr<Process> p);

  /// Called after a traced process issues a clone system call
  void handleClone(shared_ptr<Process> p, int flags);

  /// Called after a traced process issues a fork system call
  void handleFork(shared_ptr<Process> p);

  /// Called when a traced process exits
  void handleExit(shared_ptr<Process> p);

 private:
  /// The rebuild environment where this tracer will resolve artifacts
  Rebuild& _rebuild;

  /// A map from process IDs to processes. Note that a process will appear multiple times if it uses
  /// multiple threads; all entries will point to the same Process instance.
  map<pid_t, shared_ptr<Process>> _processes;
};
