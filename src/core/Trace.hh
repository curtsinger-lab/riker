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
  Trace(const Trace& other) noexcept :
      _root(other._root), _stdout(other._stdout), _stderr(other._stderr) {}

  // No need for copy assignment
  Trace& operator=(const Trace&) = delete;

 public:
  using StepList = list<tuple<shared_ptr<Command>, shared_ptr<Step>>>;

  /// Create a default trace
  Trace() noexcept;

  // Allow move
  Trace(Trace&&) noexcept = default;
  Trace& operator=(Trace&&) noexcept = default;

  /// Make a clean trace with the same initial references, but no commands or steps.
  /// This relies on the private copy constructor above.
  shared_ptr<Trace> restart() noexcept { return shared_ptr<Trace>(new Trace(*this)); }

  /// Resolve references to root, cwd, stdin, stdout, etc. in the given environment
  void resolveRefs(Build& build, shared_ptr<Env> env) noexcept;

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
  /// A reference to the initial root directory
  shared_ptr<Ref> _root;

  // Refs to the standard pipes
  shared_ptr<Ref> _stdout;
  shared_ptr<Ref> _stderr;

  /// A set of all the commands that appear in this trace
  set<shared_ptr<Command>> _commands;

  /// A sequence of tuples containing a command and a step performed by that command
  StepList _steps;

  // Declare fields for serialization
  SERIALIZE(_stdout, _stderr, _root, _commands, _steps);
};