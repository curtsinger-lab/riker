#pragma once

#include <memory>
#include <ostream>
#include <set>

#include "core/Build.hh"

using std::dynamic_pointer_cast;
using std::endl;
using std::ostream;
using std::set;
using std::shared_ptr;

/**
 * An instance of this class is used to gather statistics as it traverses a build.
 * Usage:
 */
class TraceVisitor {
 public:
  /**
   * Print the complete trace for a build
   * \param b               The build to print
   */
  TraceVisitor(Build& b) : _build(b) {}

  /// Print the trace from the given build
  void print(ostream& o) {
    // TODO: Print initial references to stdin, stdout, and stderr
    for (auto s : _build.getDefaultReferences()) {
      o << s << endl;
    }
    visit(o, _build.getRoot());
  }

  friend ostream& operator<<(ostream& o, TraceVisitor v) {
    v.print(o);
    return o;
  }

 private:
  void visit(ostream& o, shared_ptr<Command> c) {
    // Print the command's name
    o << c << endl;

    list<shared_ptr<Command>> children;

    // Print the trace for this command
    for (auto& s : c->getSteps()) {
      o << "  " << s << endl;

      // If this is a LAUNCH step, we will need to print the child command
      auto launch = std::dynamic_pointer_cast<Launch>(s);
      if (launch) {
        children.push_back(launch->getCommand());
      }
    }

    // Print traces for all child commands
    for (auto& child : children) {
      visit(o, child);
    }
  };

 private:
  Build& _build;  //< The build we're printing
};
