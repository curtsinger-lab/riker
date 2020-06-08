#pragma once

#include <memory>
#include <ostream>
#include <set>

#include "build/BuildObserver.hh"
#include "data/IR.hh"
#include "data/Version.hh"

using std::dynamic_pointer_cast;
using std::endl;
using std::ostream;
using std::set;
using std::shared_ptr;

/**
 * An instance of this class is used to gather statistics as it traverses a build.
 * Usage:
 */
class Trace final : public BuildObserver {
 public:
  /// The root command is being launched.
  virtual void launchRootCommand(const shared_ptr<Command>& root) override {
    _commands.insert(root);
  }

  /// A child command is being launched. Record the new command.
  virtual void launchChildCommand(const shared_ptr<Command>& parent,
                                  const shared_ptr<Command>& child) override {
    _commands.insert(child);
  }

  /// Print the trace from the given build
  ostream& print(ostream& o) const {
    for (auto& c : _commands) {
      o << c << endl;
      for (auto& s : c->getSteps()) {
        o << "  " << s << endl;
      }
    }
    return o;
  }

  /// Print a Trace reference
  friend ostream& operator<<(ostream& o, Trace& t) { return t.print(o); }

  // Print a Trace pointer
  friend ostream& operator<<(ostream& o, Trace* t) { return t->print(o); }

 private:
  /// The set of all commands
  set<shared_ptr<Command>> _commands;
};
