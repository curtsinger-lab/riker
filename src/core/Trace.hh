#pragma once

#include <list>
#include <memory>
#include <ostream>
#include <set>
#include <tuple>

#include "core/IR.hh"
#include "util/log.hh"
#include "util/serializer.hh"

using std::list;
using std::ostream;
using std::set;
using std::shared_ptr;
using std::tuple;

class Access;
class Build;
class Command;
class Env;
class Pipe;
class Step;

/**
 * A Trace instance captures a full trace of a build execution, and tracks each command that runs as
 * part of that build.
 */
class Trace {
 private:
  // Private copy constructor allowed. Does not copy the commands or step list
  Trace(const Trace& other) noexcept {}

  // No need for copy assignment
  Trace& operator=(const Trace&) = delete;

 public:
  using StepList = list<tuple<shared_ptr<Command>, shared_ptr<Step>>>;

  /// Create an empty trace
  Trace() noexcept = default;

  /// Initialize a default trace
  static shared_ptr<Trace> getDefault() noexcept;

  // Allow move
  Trace(Trace&&) noexcept = default;
  Trace& operator=(Trace&&) noexcept = default;

  /// Add a command to the trace
  void addCommand(shared_ptr<Command> c) noexcept { _commands.insert(c); }

  /// Get the set of commands in this trace
  const set<shared_ptr<Command>>& getCommands() const noexcept { return _commands; }

  /// Add a step to the trace
  void addStep(shared_ptr<Command> c, shared_ptr<Step> s, bool emulated) noexcept {
    _steps.emplace_back(c, s);
    LOG(ir) << c << ": " << (emulated ? "emulated " : "traced ") << s;
  }

  /// Get the list of IR steps in this trace
  const StepList& getSteps() const noexcept { return _steps; }

  /// Print this trace
  ostream& print(ostream& o) const noexcept;

  friend ostream& operator<<(ostream& o, const Trace& t) noexcept { return t.print(o); }

  friend ostream& operator<<(ostream& o, const Trace* t) noexcept {
    if (t == nullptr) return o << "<null Trace>";
    return t->print(o);
  }

 private:
  /// A set of all the commands that appear in this trace
  set<shared_ptr<Command>> _commands;

  /// A sequence of tuples containing a command and a step performed by that command
  StepList _steps;

  // Declare fields for serialization
  SERIALIZE(_commands, _steps);
};