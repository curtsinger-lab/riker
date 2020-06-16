#pragma once

#include <list>
#include <memory>
#include <tuple>

#include "util/log.hh"
#include "util/serializer.hh"

using std::list;
using std::shared_ptr;
using std::tuple;

class Access;
class Command;
class Env;
class Pipe;
class Step;

/**
 * A Trace instance captures a full trace of a build execution, and tracks each command that runs as
 * part of that build.
 */
class Trace {
 public:
  using StepList = list<tuple<shared_ptr<Command>, shared_ptr<Step>>>;

  /// Create a trace with the default starting state
  static shared_ptr<Trace> getDefault() noexcept;

  /// Clear this trace's list of steps and return the old list
  StepList reset() noexcept {
    StepList result;
    _steps.swap(result);
    return result;
  }

  // Disallow copy
  Trace(const Trace&) = delete;
  Trace& operator=(const Trace&) = delete;

  // Allow move
  Trace(Trace&&) noexcept = default;
  Trace& operator=(Trace&&) noexcept = default;

  /// Resolve references to root, cwd, stdin, stdout, etc. in the given environment
  void resolveReferences(Env& env) noexcept;

  /// Add a step to the trace
  void addStep(shared_ptr<Command> c, shared_ptr<Step> s) noexcept { _steps.emplace_back(c, s); }

  /// Get the list of IR steps in this trace
  const StepList& getSteps() const noexcept { return _steps; }

 private:
  /// Initialize this trace as a default trace
  void init() noexcept;

  /// A reference to the initial root directory
  shared_ptr<Access> _root;

  /// A reference to the initial working directory
  shared_ptr<Access> _cwd;

  /// A reference to the first executable that runs in the trace
  shared_ptr<Access> _exe;

  // References to the standard pipes
  shared_ptr<Pipe> _stdin;
  shared_ptr<Pipe> _stdout;
  shared_ptr<Pipe> _stderr;

  /// A sequence of tuples containing a command and a step performed by that command
  StepList _steps;

  Trace() = default;
  SERIALIZE(_root, _cwd, _exe, _stdin, _stdout, _stderr, _steps);
};