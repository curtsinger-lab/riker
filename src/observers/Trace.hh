#pragma once

#include <memory>
#include <ostream>
#include <set>

#include "build/BuildObserver.hh"
#include "core/IR.hh"
#include "versions/Version.hh"

using std::dynamic_pointer_cast;
using std::endl;
using std::ostream;
using std::set;
using std::shared_ptr;

/**
 * An instance of this class is used to gather statistics as it traverses a build.
 * Usage:
 */
class TraceObserver final : public BuildObserver {
 public:
  /// A child command is being launched. Record the new command.
  virtual void launch(shared_ptr<Command> parent,
                      shared_ptr<Command> child) noexcept override final {
    _commands.insert(child);
  }

  /// Print the trace from the given build
  ostream& print(ostream& o) const noexcept {
    for (const auto& c : _commands) {
      o << c << endl;
      // for (const auto& s : c->getSteps()) {
      //  o << "  " << s << endl;
      //}
    }
    return o;
  }

  /// Print a Trace reference
  friend ostream& operator<<(ostream& o, TraceObserver& t) noexcept { return t.print(o); }

  // Print a Trace pointer
  friend ostream& operator<<(ostream& o, TraceObserver* t) noexcept { return t->print(o); }

 private:
  /// The set of all commands
  set<shared_ptr<Command>> _commands;
};
